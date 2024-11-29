-module(eLog).

-include("lgDef.hrl").
-include("lgCom.hrl").

-compile(inline).
-compile({inline_size, 150}).

%% API
-export([
   %% start stop
   start/0
   , stop/0

   %% log and log param
   , dispatchLog/11
   , doLogImpl/11
   , getMd/0
   , setMd/1
   , getMdPd/0
   , getLogLevel/1
   , getLogLevel/2
   , setLogLevel/2
   , setLogLevel/3
   , setLogLevel/4
   , getLogLevels/1
   , upLogLevelCfg/1
   , setLogHwm/2
   , setLogHwm/3
   , setLogHwm/4
   , rotateHandler/1
   , rotateHandler/2
   , rotateSink/1
   , rotateAll/0

   %% stack parse
   , parseStack/1
   , parseStack/3

   %% trace
   , trace/2
   , trace/3
   , traceFile/2
   , traceFile/3
   , traceFile/4
   , traceConsole/1
   , traceConsole/2
   , installTrace/2
   , installTrace/3
   , removeTrace/1
   , traceState/3
   , traceFunc/3
   , listAllSinks/0
   , clearAllTraces/0
   , clearTraceByDest/1
   , stopTrace/1
   , stopTrace/3
   , status/0
]).

-record(trace_func_state_v1, {
   pid :: undefined | pid(),
   level :: lgAtomLevel(),
   count :: infinity | pos_integer(),
   format_string :: string(),
   timeout :: infinity | pos_integer(),
   started = os:timestamp() :: erlang:timestamp() %% use os:timestamp for compatability
}).

start() ->
   application:ensure_all_started(eLog).

stop() ->
   application:stop(eLog).

-spec dispatchLog(atom(), lgAtomLevel(), pid(), atom(), atom(), integer(), list(), string(), list() | none, pos_integer(), safe | unsafe) -> ok.
dispatchLog(Sink, Severity, Pid, Module, Function, Line, Metadata, Format, Args, Size, Safety) ->
   ?eLogCfg:get(Sink) band Severity == Severity andalso doLogImpl(Severity, Pid, Module, Function, Line, Metadata, Format, Args, Size, Sink, Safety).

doLogImpl(Severity, Pid, Module, Function, Line, Metadata, Format, Args, Size, Sink, Safety) ->
   % TraceFilters = lgConfig:ptGet({Sink, trace}, []),  %% 这个功能还没用上
   % Destinations = [], %  %% 这个功能还没用上 ?lgCASE(TraceFilters =/= [], lgUtil:check_traces(Metadata, Severity, TraceFilters, []), []),
   NowMs = lgTime:nowMs(),
   LgMsg = #lgMsg{severity = Severity, pid = Pid, module = Module, function = Function, line = Line, metadata = Metadata, timestamp = NowMs, msgFormat = Format, msgArgs = Args, msgSafety = Safety, msgFormatSize = Size},
   case lgConfig:ptGet({Sink, async}, true) of
      true ->
         gen_emm:info_notify(Sink, {mWriteLog, LgMsg});
      _ ->
         gen_emm:call_notify(Sink, {mWriteLog, LgMsg})
   end,
   
   %% 这个功能还没用上
   % case whereis(?LgTrackSink) of
   %    undefined ->
   %       ok;
   %    TraceSinkPid ->
   %       gen_emm:info_notify(TraceSinkPid, {mWriteLog, LgMsg})
   % end.
   ok.

%% @doc Get metadata for current process
-spec getMd() -> [{atom(), any()}].
getMd() ->
   case erlang:get(?PdMdKey) of
      undefined -> [];
      MD -> MD
   end.

%% @doc Get metadata for current process
-spec getMdPd() -> term().
getMdPd() ->
   erlang:get(?PdMdKey).

%% @doc Set metadata for current process.
%% Will badarg if you don't supply a list of {key, value} tuples keyed by atoms.
-spec setMd([{atom(), any()}, ...]) -> ok.
setMd(NewMD) when is_list(NewMD) ->
   %% make sure its actually a real proplist
   case lists:all(
      fun({Key, _Value}) when is_atom(Key) -> true;
         (_) -> false
      end, NewMD) of
      true ->
         erlang:put(?PdMdKey, NewMD),
         ok;
      _ ->
         erlang:error(badarg)
   end;
setMd(_) ->
   erlang:error(badarg).

%% @doc Set the loglevel for a particular backend.
setLogLevel(Handler, Level) ->
   setLogLevel(?LgDefSink, Handler, undefined, Level).

%% @doc Set the loglevel for a particular backend that has multiple identifiers (eg. the file backend).
setLogLevel(Handler, Ident, Level) ->
   setLogLevel(?LgDefSink, Handler, Ident, Level).

%% @doc Set the loglevel for a particular sink's backend that potentially has multiple identifiers. (Use `undefined' if it doesn't have any.)
setLogLevel(Sink, Handler, Ident, Level) when is_atom(Level) ->
   HandlerArg =
      case Ident of
         undefined -> Handler;
         _ -> {Handler, Ident}
      end,
   Reply = gen_emm:call(Sink, HandlerArg, {mSetLogLevel, Level}),
   upLogLevelCfg(Sink),
   Reply.


%% @doc Get the loglevel for a particular backend on the default sink. In the case that the backend has multiple identifiers, the lowest is returned.
getLogLevel(Handler) ->
   getLogLevel(?LgDefSink, Handler).

%% @doc Get the loglevel for a particular sink's backend. In the case that the backend
%% has multiple identifiers, the lowest is returned.
getLogLevel(Sink, Handler) ->
   case gen_emm:call(Sink, Handler, mGetLogLevel) of
      Mask when is_integer(Mask) ->
         case lgUtil:maskToLevels(Mask) of
            [] -> none;
            Levels -> hd(Levels)
         end;
      Y -> Y
   end.

getLogLevels(Sink) ->
   [gen_emm:call(Sink, Handler, mGetLogLevel) || Handler <- gen_emm:which_epm(Sink)].

%% @doc Set the loghwm for the default sink.
setLogHwm(Handler, Hwm) when is_integer(Hwm) ->
   setLogHwm(?LgDefSink, Handler, Hwm).

%% @doc Set the loghwm for a particular backend.
setLogHwm(Sink, Handler, Hwm) when is_integer(Hwm) ->
   gen_emm:call(Sink, Handler, {mSetLogHwm, Hwm}).

%% @doc Set the loghwm (log high water mark) for file backends with multiple identifiers
setLogHwm(Sink, Handler, Ident, Hwm) when is_integer(Hwm) ->
   gen_emm:call(Sink, {Handler, Ident}, {mSetLogHwm, Hwm}).

%% @doc recalculate min log level
upLogLevelCfg(error_logger) ->
   %% Not a sink under our control, part of the Erlang logging
   %% utility that lg attaches to
   true;
upLogLevelCfg(Sink) ->
   Traces = lgConfig:ptGet({Sink, trace}, []),
   case Traces of
      [] ->
         AllLogLevel = allLogLevel(getLogLevels(Sink), 0),
         ets:insert(?eLogEts, {Sink, AllLogLevel}),
         AllSinks = ets:tab2list(?eLogEts),
         lgKvsToBeam:load(?eLogCfg, AllSinks);
      _ ->
         ets:insert(?eLogEts, {Sink, 16#ff}),
         AllSinks = ets:tab2list(?eLogEts),
         lgKvsToBeam:load(?eLogCfg, AllSinks)
   end.

allLogLevel([], Acc) ->
   Acc;
allLogLevel([OneLv | Levels], Acc) ->
   allLogLevel(Levels, OneLv bor Acc).

rotateSink(Sink) ->
   Handlers = lgConfig:ptGet(handlers, []),
   RotateHandlers = lists:filtermap(
      fun({Handler, _, S}) when S == Sink -> {true, {Handler, Sink}}; (_) -> false end, Handlers),
   rotateHandlers(RotateHandlers).

rotateAll() ->
   rotateHandlers(lists:map(fun({H, _, S}) -> {H, S} end, lgConfig:ptGet(handlers, []))).

rotateHandlers(Handlers) ->
   [rotateHandler(Handler, Sink) || {Handler, Sink} <- Handlers].

rotateHandler(Handler) ->
   Handlers = lgConfig:ptGet(handlers, []),
   case lists:keyfind(Handler, 1, Handlers) of
      {Handler, _, Sink} -> rotateHandler(Handler, Sink);
      false -> ok
   end.

rotateHandler(Handler, Sink) ->
   gen_emm:call(Sink, Handler, mRotate, ?LgRotateTimeout).

%% @doc Print stacktrace in human readable form
parseStack(Stacktrace) ->
   <<
      begin
         case Location of
            [] ->
               <<"     ", (atom_to_binary(Mod, utf8))/binary, ":", (atom_to_binary(Func, utf8))/binary, "(", (eFmt:format(<<"~w">>, [Arity]))/binary, ")\n">>;
            [{file, File}, {line, Line}] ->
               <<"     ", (atom_to_binary(Mod, utf8))/binary, ":", (atom_to_binary(Func, utf8))/binary, "/", (integer_to_binary(Arity))/binary, "(", (unicode:characters_to_binary(File))/binary, ":", (integer_to_binary(Line))/binary, ")\n">>;
            _ ->
               <<"     ", (atom_to_binary(Mod, utf8))/binary, ":", (atom_to_binary(Func, utf8))/binary, "(", (eFmt:format(<<"~w">>, [Arity]))/binary, ")", (eFmt:format(<<"~w">>, [Location]))/binary, "\n">>
         end
      end || {Mod, Func, Arity, Location} <- Stacktrace
>>.

parseStack(Class, Reason, Stacktrace) ->
   eFmt:format(<<"~n  Class:~p~n  Reason:~p~n  Stacktrace:~s">>, [Class, Reason, parseStack(Stacktrace)]).

trace(BkdMod, Filter) ->
   trace(BkdMod, Filter, debug).

trace({lgBkdFile, File}, Filter, Level) ->
   traceFile(File, Filter, Level);
trace(Backend, Filter, Level) ->
   case validateTraceFilters(Filter, Level, Backend) of
      {Sink, {ok, Trace}} ->
         add_trace_to_loglevel_config(Trace, Sink),
         {ok, {Backend, Filter, Level}};
      {_Sink, Error} ->
         Error
   end.

traceFile(File, Filter) ->
   traceFile(File, Filter, debug, []).

traceFile(File, Filter, Level) when is_atom(Level) ->
   traceFile(File, Filter, Level, []);

traceFile(File, Filter, Options) when is_list(Options) ->
   traceFile(File, Filter, debug, Options).

traceFile(File, Filter, Level, Options) ->
   FileName = lgUtil:parsePath(File),
   case validateTraceFilters(Filter, Level, {lgBkdFile, FileName}) of
      {Sink, {ok, Trace}} ->
         Handlers = lgConfig:ptGet(handlers, []),
         %% check if this file backend is already installed
         Res =
            case lgUtil:find_file(FileName, Handlers) of
               false ->
                  %% install the handler
                  LogFileConfig =
                     lists:keystore(level, 1,
                        lists:keystore(file, 1,
                           Options,
                           {file, FileName}),
                        {level, none}),
                  HandlerInfo =
                     eLog_app:startHandler(Sink, {lgBkdFile, FileName}, LogFileConfig),
                  lgConfig:ptSet(handlers, [HandlerInfo | Handlers]),
                  {ok, installed};
               {_Watcher, _Handler, Sink} ->
                  {ok, exists};
               {_Watcher, _Handler, _OtherSink} ->
                  {error, file_in_use}
            end,
         case Res of
            {ok, _} ->
               add_trace_to_loglevel_config(Trace, Sink),
               {ok, {{lgBkdFile, FileName}, Filter, Level}};
            {error, _} = E ->
               E
         end;
      {_Sink, Error} ->
         Error
   end.

traceConsole(Filter) ->
   traceConsole(Filter, debug).

traceConsole(Filter, Level) ->
   trace(lgBkdConsole, Filter, Level).

stopTrace(Backend, Filter, Level) ->
   case validateTraceFilters(Filter, Level, Backend) of
      {Sink, {ok, Trace}} ->
         stopTraceInt(Trace, Sink);
      {_Sink, Error} ->
         Error
   end.

stopTrace({Backend, Filter, Level}) ->
   stopTrace(Backend, Filter, Level).


validateTraceFilters(Filters, Level, Backend) ->
   Sink = proplists:get_value(sink, Filters, ?LgDefSink),
   {Sink, lgUtil:validate_trace({proplists:delete(sink, Filters), Level, Backend})}.

%% Important: validate_trace_filters orders the arguments of
%% trace tuples differently than the way outside callers have
%% the trace tuple.
%%
%% That is to say, outside they are represented as 
%% `{Backend, Filter, Level}'
%%
%% and when they come back from validation, they're
%% `{Filter, Level, Backend}'
stopTraceInt({_Filter, _Level, Backend} = Trace, Sink) ->
   Traces = lgConfig:ptGet({Sink, trace}, []),
   NewTraces = lists:delete(Trace, Traces),
   _ = lgUtil:trace_filter([element(1, T) || T <- NewTraces]),
   %MinLevel = minimum_loglevel(get_loglevels() ++ get_trace_levels(NewTraces)),
   lgConfig:ptSet({Sink, trace}, NewTraces),
   eLog:upLogLevelCfg(Sink),

   case getLogLevel(Sink, Backend) of
      none ->
         %% check no other traces point here
         case lists:keyfind(Backend, 3, NewTraces) of
            false ->
               gen_emm:delEpm(Sink, Backend, []),
               lgConfig:ptSet(handlers, lists:keydelete(Backend, 1, lgConfig:ptGet(handlers, [])));
            _ ->
               ok
         end;
      _ ->
         ok
   end,
   ok.

%% @doc installs a eLog trace handler into the target process (using sys:install) at the specified level.
-spec installTrace(pid(), lgAtomLevel()) -> ok.
installTrace(Pid, Level) ->
   installTrace(Pid, Level, []).

-spec installTrace(pid(), lgAtomLevel(), [{count, infinity | pos_integer()} | {format_string, string()} | {timeout, timeout()}]) -> ok.
installTrace(Pid, Level, Options) ->
   sys:install(Pid, {fun ?MODULE:traceFunc/3, traceState(Pid, Level, Options)}).

%% @doc remove a previously installed eLog trace handler from the target process.
-spec removeTrace(pid()) -> ok.
removeTrace(Pid) ->
   sys:remove(Pid, fun ?MODULE:traceFunc/3).

listAllSinks() ->
   sets:to_list(
      lists:foldl(
         fun({_Watcher, _Handler, Sink}, Set) ->
            sets:add_element(Sink, Set)
         end, sets:new(), lgConfig:ptGet(handlers, [])
      )
   ).

clearTracesBySink(Sinks) ->
   lists:foreach(
      fun(S) ->
         lgConfig:ptSet({S, trace}, []),
         eLog:upLogLevelCfg(S)
      end,
      Sinks).


clearTraceByDest(ID) ->
   Sinks = lists:sort(listAllSinks()),
   Traces = findTraces(Sinks),
   [stopTraceInt({Filter, Level, Destination}, Sink) || {Sink, {Filter, Level, Destination}} <- Traces, Destination == ID].


clearAllTraces() ->
   Handlers = lgConfig:ptGet(handlers, []),
   clearTracesBySink(listAllSinks()),
   _ = lgUtil:trace_filter(none),
   lgConfig:ptSet(handlers,
      lists:filter(
         fun({Handler, _Watcher, Sink}) ->
            case getLogLevel(Sink, Handler) of
               none ->
                  gen_emm:delEpm(Sink, Handler, []),
                  false;
               _ ->
                  true
            end
         end, Handlers)).

findTraces(Sinks) ->
   lists:foldl(
      fun(S, Acc) ->
         Traces = lgConfig:ptGet({S, trace}, []),
         Acc ++ lists:map(fun(T) -> {S, T} end, Traces)
      end,
      [], Sinks).

status() ->
   Handlers = lgConfig:ptGet(handlers, []),
   Sinks = lists:sort(listAllSinks()),
   Traces = findTraces(Sinks),
   TraceCount = case length(Traces) of
                   0 -> 1;
                   N -> N
                end,
   Status = ["eLog status:\n",
      [begin
          Level = getLogLevel(Sink, Handler),
          get_sink_handler_status(Sink, Handler, Level)
       end || {Handler, _Watcher, Sink} <- lists:sort(fun({_, _, S1},
         {_, _, S2}) -> S1 =< S2 end,
         Handlers)],
      "Active Traces:\n",
      [begin
          LevelName =
             case lgUtil:maskToLevels(Level) of
                [] -> none;
                Levels -> hd(Levels)
             end,

          io_lib:format("Tracing messages matching ~p (sink ~s) at level ~p to ~p\n",
             [Filter, Sink, LevelName, Destination])
       end || {Sink, {Filter, Level, Destination}} <- Traces],
      [
         "Tracing Reductions:\n",
         case ?LgDefTracer:info('query') of
            {null, false} -> "";
            Query -> io_lib:format("~p~n", [Query])
         end
      ],
      [
         "Tracing Statistics:\n ",
         [begin
             [" ", atom_to_list(Table), ": ",
                integer_to_list(?LgDefTracer:info(Table) div TraceCount),
                "\n"]
          end || Table <- [input, output, filter]]
      ]],
   io:put_chars(Status).

get_sink_handler_status(Sink, Handler, Level) ->
   case Handler of
      {lgBkdFile, File} ->
         io_lib:format("File ~ts (~s) at level ~p\n", [File, Sink, Level]);
      lgBkdConsole ->
         io_lib:format("Console (~s) at level ~p\n", [Sink, Level]);
      _ ->
         []
   end.

%% @private
add_trace_to_loglevel_config(Trace, Sink) ->
   Traces = lgConfig:ptGet({Sink, trace}, []),
   case lists:member(Trace, Traces) of
      false ->
         NewTraces = [Trace | Traces],
         _ = lgUtil:trace_filter([element(1, T) || T <- NewTraces]),
         lgConfig:ptSet({Sink, trace}, [Trace | Traces]),
         eLog:upLogLevelCfg(Sink);
      _ ->
         ok
   end.

%% @private Print the format string `Fmt' with `Args' without a size limit.
%% This is unsafe because the output of this function is unbounded.
%%
%% Log messages with unbounded size will kill your application dead as
%% OTP mechanisms stuggle to cope with them.  So this function is
%% intended <b>only</b> for messages which have a reasonable bounded
%% size before they're formatted.
%%
%% If the format string is invalid or not enough arguments are
%% supplied a 'FORMAT ERROR' message is printed instead with the
%% offending arguments. The caller is NOT crashed.

%% @private
traceFunc(#trace_func_state_v1{pid = Pid, level = Level, format_string = Fmt} = FuncState, Event, ProcState) ->
   _ = eLog:log(Level, Pid, Fmt, [Event, ProcState]),
   check_timeout(decrement_count(FuncState)).

%% @private
traceState(Pid, Level, Options) ->
   #trace_func_state_v1{pid = Pid,
      level = Level,
      count = proplists:get_value(count, Options, infinity),
      timeout = proplists:get_value(timeout, Options, infinity),
      format_string = proplists:get_value(format_string, Options, "TRACE ~p ~p")}.

decrement_count(#trace_func_state_v1{count = infinity} = FuncState) ->
   FuncState;
decrement_count(#trace_func_state_v1{count = 1}) ->
   %% hit the counter limit
   done;
decrement_count(#trace_func_state_v1{count = Count} = FuncState) ->
   FuncState#trace_func_state_v1{count = Count - 1}.

check_timeout(#trace_func_state_v1{timeout = infinity} = FuncState) ->
   FuncState;
check_timeout(#trace_func_state_v1{timeout = Timeout, started = Started} = FuncState) ->
   case (timer:now_diff(os:timestamp(), Started) / 1000) > Timeout of
      true ->
         done;
      false ->
         FuncState
   end.