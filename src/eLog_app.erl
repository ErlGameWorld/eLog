-module(eLog_app).

-behaviour(application).

-include("lgCom.hrl").
-include("lgDef.hrl").

-export([
   start/2
   , stop/1
   , prep_stop/1
   , doStart/0
   , startSink/1
   , startSink/2
   , startHandler/3
]).

start(_StartType, _StartArgs) ->
   {ok, Pid} = eLog_sup:start_link(),
   SavedHandlers = doStart(),
   doStartExtraSink(),
   doStartTraces(),
   {ok, Pid, SavedHandlers}.

%% 启动默认的接收器(sink)
doStart() ->
   %% 尝试启动异步管理者
   tryStartAsyncMgr(lgUtil:get_env(asyncThreshold, undefined), lgUtil:get_env(asyncThrWindow, undefined), ?LgDefSink),

   %%尝试安装killer
   tryInstallKiller(lgUtil:get_env(killerHwm, undefined), lgUtil:get_env(killerReTime, undefined), ?LgDefSink),

   %%尝试启动各个handler
   tryStartHandlers(lgUtil:get_env(handlers, ?LgDefHandler), ?LgDefSink),
   %% 尝试替换error logger
   SavedHandlers = tryStartErrLoggerHandler(lgUtil:get_env(errLoggerRedirect, true), lgUtil:get_env(errLoggerHwm, undefined), lgUtil:get_env(errLoggerWhitelist, [])),
   eLog:upLogLevelCfg(?LgDefSink),
   SavedHandlers.

startSink(?LgDefSink) -> doStart();
startSink(Sink) ->
   AllSinksDef = lgUtil:get_env(extraSinks, []),
   SinkValue = lists:keyfind(Sink, 1, AllSinksDef),
   SinkOpts = ?lgCASE(SinkValue == false, [], element(2, SinkValue)),
   startSink(Sink, SinkOpts).

startSink(Sink, Opts) ->
   ChildId = lgUtil:makeInnerSinkName(Sink),
   SinkSpec = #{
      id => ChildId,
      start => {gen_emm, start_link, [{local, Sink}]},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [dynamic]},
   _ = supervisor:start_child(eLog_sup, SinkSpec),
   tryStartAsyncMgr(proplists:get_value(asyncThreshold, Opts, undefined), proplists:get_value(asyncThrWindow, Opts, undefined), Sink),
   tryInstallKiller(proplists:get_value(killerHwm, Opts, undefined), proplists:get_value(killerReTime, Opts, undefined), Sink),
   tryStartHandlers(proplists:get_value(handlers, Opts, []), Sink),
   eLog:upLogLevelCfg(Sink).

doStartExtraSink() ->
   doStartExtraSinks(lgUtil:get_env(extraSinks, [])).

doStartExtraSinks(Sinks) ->
   [startSink(Sink, Opts) || {Sink, Opts} <- Sinks],
   ok.

doStartTraces() ->
   _ = lgUtil:trace_filter(none),
   ok = addTraces().

addTraces() ->
   Traces = lgUtil:get_env(traces, []),
   [startTrace(One) || One <- Traces],
   ok.

startTrace({Handler, Filter}) ->
   {ok, _} = eLog:trace(Handler, Filter);
startTrace({Handler, Filter, Level}) when is_atom(Level) ->
   {ok, _} = eLog:trace(Handler, Filter, Level).

prep_stop(Handlers) ->
   error_logger:delete_report_handler(lgErrLoggerH),
   [error_logger:add_report_handler(Handler) || Handler <- Handlers],
   case lgUtil:get_env(errLoggerRedirect, true) of
      true ->
         logger:add_handlers(kernel);
      _ ->
         ignore
   end,
   ok.

stop(_State) ->
   ok.

tryStartAsyncMgr(undefined, _Window, _Sink) ->
   ignore;
tryStartAsyncMgr(Threshold, Window, Sink) ->
   case Window of
      undefined ->
         supervisor:start_child(lgHWatcherSup, [Sink, lgBkdThrottle, [Sink, Threshold, erlang:trunc(Threshold * 0.2)]]);
      _ ->
         supervisor:start_child(lgHWatcherSup, [Sink, lgBkdThrottle, [Sink, Threshold, Window]])
   end,
   ok.

tryInstallKiller(undefined, _ReTime, _Sink) -> ok;
tryInstallKiller(Hwm, ReTime, Sink) ->
   case ReTime of
      undefined ->
         _ = supervisor:start_child(lgHWatcherSup, [Sink, lgMgrKiller, [Hwm, 5000]]);
      _ ->
         _ = supervisor:start_child(lgHWatcherSup, [Sink, lgMgrKiller, [Hwm, ReTime]])
   end,
   ok.

tryStartHandlers(undefined, _Sink) -> ok;
tryStartHandlers(Handlers, Sink) ->
   %% 启动失败的处理程序将在handler_watcher中处理
   NewHandler = doStartHandlers(Handlers, Sink, [], []),
   lgConfig:ptSet(handlers, lgConfig:ptGet(handlers, []) ++ NewHandler),
   ok.

doStartHandlers([], _Sink, _NameAcc, HandlerAcc) ->
   HandlerAcc;
doStartHandlers([OneHandler | Handlers], Sink, NameAcc, HandlerAcc) ->
   {Module, Options} = tryMakeHandlerId(OneHandler),
   NewNameAcc =
      case Module of
         {lgBkFile, FId} ->
            case lists:member(FId, NameAcc) of
               true ->
                  error_logger:error_msg(<<"Cannot have same file (~p) in multiple file backends~n">>, [FId]),
                  throw({error, bad_config});
               _ ->
                  [FId | NameAcc]
            end;
         _ ->
            NameAcc
      end,

   HandlerRet = startHandler(Sink, Module, Options),
   doStartHandlers(Handlers, Sink, NewNameAcc, [HandlerRet | HandlerAcc]).

startHandler(Sink, Module, Config) ->
   {ok, Watcher} = supervisor:start_child(lgHWatcherSup, [Sink, Module, Config]),
   {Module, Watcher, Sink}.

-spec tryStartErrLoggerHandler(boolean(), pos_integer(), list()) -> list().
tryStartErrLoggerHandler(false, _Hwm, _Whitelist) -> [];
tryStartErrLoggerHandler(_ErrLoggerRedirect, Hwm, WhiteList) ->
   case whereis(error_logger) of
      undefined ->
         %% 在OTP 21及以上版本中，error_logger已弃用，而改用 'logger'
         %% 作为一个修补, 启动error_logger并将其安装为 logger handler
         %% 我们不能使用 error_logger:add_report_handler 因为我们想要监视这个handler
         %% 因此，我们必须手动添加这个 logger handler
         %%
         %% 从长远来看，我们应该安装一个日志处理程序, but this will bridge the gap for now.
         _ = error_logger:start(),
         _ = logger:add_handler(error_logger, error_logger, #{level => info, filter_default => log}),
         ok = tryRemoveLoggerHandler();
      _ ->
         ok = tryRemoveLoggerHandler(),
         ok
   end,

   %% capture which handlers we removed from error_logger so we can restore them when eLog stops
   %% 捕获从error_logger中删除的处理程序，以便在eLog停止时恢复它们
   case supervisor:start_child(lgHWatcherSup, [error_logger, lgErrLoggerH, [Hwm, lgUtil:get_env(errLoggerGLS, handle)]]) of
      {ok, _} ->
         [begin error_logger:delete_report_handler(X), X end || X <- gen_event:which_handlers(error_logger) -- [lgErrLoggerH | WhiteList]];
      {error, _} ->
         []
   end.

%% 在OTP 21.1及更高版本上，我们需要删除`default' handler。但是它可能不存在，因此我们将其包装在try-catch块中
tryRemoveLoggerHandler() ->
   try
      ok = logger:remove_handler(default)
   catch
      error:undef -> ok;
      Err:Reason ->
         error_logger:error_msg(<<"calling logger:remove_handler(default) failed: ~p ~p">>, [Err, Reason])
   end.

tryMakeHandlerId({Mod, Config}) ->
   %%允许后端生成gen_event处理程序id，如果它愿意的话。
   %%这里我们没有使用erlang:function_exported，因为这需要用到模块
   %%已经加载，这在启动阶段是不太可能的。使用代码:负载
   %%会在生成代码覆盖率报告时产生不良的副作用。
   try Mod:configToId(Config) of
      Id ->
         {Id, Config}
   catch
      error:undef ->
         {Mod, Config}
   end.