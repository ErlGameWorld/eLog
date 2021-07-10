-module(lgHWatcherSrv).

-behaviour(gen_srv).

-include("lgDef.hrl").

-export([
   start/3
   , start_link/3
]).

-export([
   init/1
   , handleCall/3
   , handleCast/2
   , handleInfo/2
   , terminate/2
   , code_change/3
]).

-record(state, {
   module :: atom(),
   config :: any(),
   sink :: pid() | atom()
}).

start(Sink, Module, Config) ->
   gen_srv:start(?MODULE, {Sink, Module, Config}, []).

start_link(Sink, Module, Config) ->
   gen_srv:start_link(?MODULE, {Sink, Module, Config}, []).

init({Sink, Module, Config}) ->
   process_flag(trap_exit, true),
   installHandler(Module, Config, Sink),
   {ok, #state{sink = Sink, module = Module, config = Config}}.

handleCall(_Msg, _State, _From) ->
   ?ERR(<<"~p call receive unexpect msg ~p ~n ">>, [?MODULE, _Msg]),
   {reply, ok}.

handleCast(_Msg, _State) ->
   ?ERR(<<"~p cast receive unexpect msg ~p ~n ">>, [?MODULE, _Msg]),
   kpS.

handleInfo({gen_event_EXIT, Module, normal}, #state{module = Module} = State) ->
   {stop, normal, State};
handleInfo({gen_event_EXIT, Module, shutdown}, #state{module = Module} = State) ->
   {stop, normal, State};
handleInfo({gen_event_EXIT, Module, {'EXIT', {kill_me, [_KillerHwm, KillerReinstallAfter]}}}, #state{module = Module, sink = Sink, config = Config} = State) ->
   %% Brutally kill the manager but stay alive to restore settings.
   %%
   %% SinkPid here means the gen_event process. Handlers *all* live inside the
   %% same gen_event process space, so when the Pid is killed, *all* of the
   %% pending log messages in its mailbox will die too.
   SinkPid = whereis(Sink),
   unlink(SinkPid),
   {_, Len} = process_info(SinkPid, message_queue_len),
   error_logger:error_msg(<<"Killing sink ~p, current message_queue_len:~p~n">>, [Sink, Len]),
   exit(SinkPid, kill),
   _ = timer:apply_after(KillerReinstallAfter, eLog_app, startHandler, [Sink, Module, Config]),
   {stop, normal, State};
handleInfo({gen_event_EXIT, Module, Reason}, #state{module = Module, config = Config, sink = Sink} = State) ->
   case ?LgShouldLog(?error) of
      true ->
         ?LgNotify(?error, self(), <<"eLog event handler ~p exited with reason ~s">>, [Module, lgErrLoggerH:formatReason(Reason)]),
         installHandler(Module, Config, Sink);
      _ ->
         ok
   end,
   {noreply, State};
handleInfo(mReInstallHandler, #state{module = Module, config = Config, sink = Sink}) ->
   installHandler(Module, Config, Sink),
   kpS;
handleInfo({reboot, Sink}, _State) ->
   eLog_app:startSink(Sink),
   kpS;
handleInfo(stop, State) ->
   {stop, normal, State};
handleInfo({'EXIT', _Pid, killed}, #state{module = Module, config = Config, sink = Sink} = State) ->
   Tmr = lgUtil:get_env(killerReTime, 5000),
   _ = timer:apply_after(Tmr, eLog_app, startHandler, [Sink, Module, Config]),
   {stop, normal, State};
handleInfo(_Msg, _State) ->
   ?ERR(<<"~p info receive unexpect msg ~p ~n ">>, [?MODULE, _Msg]),
   kpS.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

installHandler(Module, Config, Sink) ->
   Ret =
      case Module of
         lgErrLoggerH ->
            gen_event:add_sup_handler(Sink, Module, Config);
         lgBkThrottle ->
            %% lgBkdThrottle需要知道它连接到哪个接收器，因此这个公认的丑陋的解决方案。处理程序对发送到' init'的配置结构是敏感的，
            %% 遗憾的是，在不破坏第三方处理程序的情况下，向后端添加一个要忽略的配置项并不是一件小事。
            gen_emm:add_sup_epm(Sink, Module, Config);
         _ ->
            gen_emm:add_sup_epm(Sink, Module, Config)
      end,
   case Ret of
      ok ->
         ?INT_LOG(?debug, <<"eLog installed handler ~p into ~p ~p">>, [Module, Sink, whereis(Sink)]),
         %eLog:updateLogevelCfg(Sink),
         ok;
      {error, {fatal, Reason}} ->
         ?INT_LOG(?error, <<"eLog fatally failed to install handler ~p into ~p, NOT retrying: ~p">>, [Module, Sink, Reason]),
         %% tell ourselves to stop
         self() ! stop,
         ok;
      Error ->
         %% try to reinstall it later
         ?INT_LOG(?error, <<"eLog failed to install handler ~p into ~p, retrying later : ~p">>, [Module, Sink, Error]),
         erlang:send_after(5000, self(), mReInstallHandler),
         ok
   end.
