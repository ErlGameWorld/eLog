-module(lgBkdThrottle).

%% @doc一个简单的gen_event后端，用于监视邮箱大小和在同步和异步模式之间切换日志消息。
%%使用gen_event处理程序是因为进程获得了自己的邮箱的大小不涉及获取锁定，
%% 并且gen_event处理程序在其中运行parent's的进程里。

-behaviour(gen_emm).

-include("lgDef.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   init/1
   , handleCall/2
   , handleEvent/2
   , handleInfo/2
   , terminate/2
   , code_change/3
]).

-record(state, {
   sink :: atom(),
   hwm :: non_neg_integer(),
   window :: non_neg_integer(),
   async = true :: boolean()
}).

init([Sink, Hwm, Window]) ->
   lgConfig:ptSet({Sink, async}, true),
   {ok, #state{sink = Sink, hwm = Hwm, window = Hwm - Window}}.

handleCall(mGetLogLevel, State) ->
   {reply, ?none, State};
handleCall({mSetLogLevel, _Level}, State) ->
   {reply, ok, State};
handleCall(_Msg, State) ->
   ?ERR(<<"~p call receive unexpect msg ~p ~n ">>, [?MODULE, _Msg]),
   {reply, ok, State}.

handleEvent({mWriteLog, _Message}, #state{sink = Sink, hwm = Hwm, window = Window, async = Async} = State) ->
   {_, MsgLen} = erlang:process_info(self(), message_queue_len),
   if
      MsgLen > Hwm andalso Async ->
         %% need to flip to sync mode
         lgConfig:ptSet({Sink, async}, false),
         {ok, State#state{async = false}};
      MsgLen < Window andalso not Async ->
         %% need to flip to async mode
         lgConfig:ptSet({Sink, async}, true),
         {ok, State#state{async = true}};
      true ->
         %% nothing needs to change
         kpS
   end;
handleEvent(_Event, _State) ->
   kpS.

handleInfo(_Info, _State) ->
   kpS.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
