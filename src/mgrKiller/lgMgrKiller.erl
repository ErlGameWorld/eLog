-module(lgMgrKiller).

-behavior(gen_emm).

-include("lgDef.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([kill_me/0]).

-export([
   init/1
   , handleEvent/2
   , handleCall/2
   , handleInfo/2
   , terminate/2
   , code_change/3
]).

-record(state, {
   killerHwm :: non_neg_integer(),
   killerReTime :: non_neg_integer()
}).

kill_me() ->
   gen_emm:call(?LgDefSink, ?MODULE, mKillSelf).

init([KillerHwm, KillerReinstallAfter]) ->
   {ok, #state{killerHwm = KillerHwm, killerReTime = KillerReinstallAfter}}.

handleCall(mGetLogLevel, State) ->
   {reply, ?llvNone, State};
handleCall({mSetLogLevel, _Level}, State) ->
   {reply, ok, State};
handleCall(mGetSettings, State = #state{killerHwm = KillerHwm, killerReTime = KillerReinstallAfter}) ->
   {reply, [KillerHwm, KillerReinstallAfter], State};
handleCall(mKillSelf, #state{killerHwm = KillerHwm, killerReTime = KillerReinstallAfter}) ->
   exit({kill_me, [KillerHwm, KillerReinstallAfter]});
handleCall(_Request, State) ->
   {reply, ok, State}.

%% 检查每个日志消息的队列长度不是世界上最好的主意。我们将来可以使此操作在轮询计时器上起作用。
handleEvent({mWriteLog, _Message}, #state{killerHwm = KillerHwm, killerReTime = KillerReinstallAfter}) ->
   {_, Len} = process_info(self(), message_queue_len),
   case Len > KillerHwm of
      true ->
         exit({kill_me, [KillerHwm, KillerReinstallAfter]});
      _ ->
         kpS
   end;
handleEvent(_Event, _State) ->
   kpS.

handleInfo(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
