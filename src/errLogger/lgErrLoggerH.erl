-module(lgErrLoggerH).

%% 一个error_logger后端，用于将事件重定向到eLog。
%% 错误消息和崩溃日志也可以选择写入崩溃日志。 @see lgCrashLog

-behaviour(gen_event).

-include("lgDef.hrl").
-include("eLog.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   setHighWater/1
   , formatReason/1
   , formatMfa/1
   , formatArgs/2

   , suppress_supervisor_start_and_application_start/1
   , suppress_supervisor_start/1
   , suppress_application_start/1

]).

-export([
   init/1
   , handle_call/2
   , handle_event/2
   , handle_info/2
   , terminate/2
   , code_change/3
]).

-record(state, {
   sink :: atom(),
   shaper :: lgShaper(),
   %% group leader strategy
   groupleader_strategy :: handle | ignore | mirror,
   raw :: boolean()
}).

-define(LOGMSG(Sink, Severity, PidOrMd, Msg),
   case ?LgShouldLog(Sink, Severity) of
      true ->
         if
            is_pid(PidOrMd) ->
               eLog:doLogImpl(Severity, PidOrMd, 'Undef', 'Undef', 'Undef', 0, [], Msg, [], -1, Sink, safe);
            is_list(PidOrMd) ->
               {_, LogPid} = lists:keyfind(pid, 1, PidOrMd),
               eLog:doLogImpl(Severity, LogPid, 'Undef', 'Undef', 'Undef', 0, lists:keydelete(pid, 1, PidOrMd), Msg, [], -1, Sink, safe);
            true ->
               eLog:doLogImpl(Severity, list_to_pid("<0.0.0>"), 'Undef', 'Undef', 'Undef', 0, [], Msg, [{tag, PidOrMd}], -1, Sink, safe)
         end,
         logged;
      _ -> no_log
   end).

% doLogImpl(Severity, Pid, Node, Module, Function, Line, Metadata, Format, Args, Size, Sink, Safety)

-define(LOGFMT(Sink, Severity, PidOrMd, Fmt, Args),
   case ?LgShouldLog(Sink, Severity) of
      true ->
         if
            is_pid(PidOrMd) ->
               eLog:doLogImpl(Severity, PidOrMd, 'Undef', 'Undef', 'Undef', 0, [], Fmt, Args, -1, Sink, safe);
            is_list(PidOrMd) ->
               {_, LogPid} = lists:keyfind(pid, 1, PidOrMd),
               eLog:doLogImpl(Severity, LogPid, 'Undef', 'Undef', 'Undef', 0, lists:keydelete(pid, 1, PidOrMd), Fmt, Args, -1, Sink, safe);
            true ->
               eLog:doLogImpl(Severity, list_to_pid("<0.0.0>"), 'Undef', 'Undef', 'Undef', 0, [{tag, PidOrMd}], Fmt, Args, -1, Sink, safe)
         end,
         logged;
      _ -> no_log
   end).

-ifdef(TEST).
%% Make CRASH synchronous when testing, to avoid timing headaches
-define(CRASH_LOG(Event),
   catch (gen_srv:call(lgCrashLog, {mWriteLog, Event}))).
-else.
-define(CRASH_LOG(Event),
   gen_srv:send(lgCrashLog, {mWriteLog, Event})).
-endif.

setHighWater(N) ->
   gen_event:call(error_logger, ?MODULE, {mSetHighWater, N}).

-spec init(any()) -> {ok, #state{}}.
init([HighWaterMark, GlStrategy]) ->
   Flush = lgUtil:get_env(errLoggerFlushQueue, true),
   FlushThr = lgUtil:get_env(errLoggerFlushThr, 20),
   Shaper = #lgShaper{hwm = HighWaterMark, flushQueue = Flush, flushThr = FlushThr, filter = shaperFun(), id = ?MODULE},
   Raw = lgUtil:get_env(errLoggerFmtRaw, false),
   Sink = configSink(),
   {ok, #state{sink = Sink, shaper = Shaper, groupleader_strategy = GlStrategy, raw = Raw}}.

handle_call({mSetHighWater, N}, #state{shaper = Shaper} = State) ->
   NewShaper = Shaper#lgShaper{hwm = N},
   {ok, ok, State#state{shaper = NewShaper}};
handle_call(_Request, State) ->
   {ok, unknown_call, State}.

handle_event(Event, #state{sink = Sink, shaper = Shaper} = State) ->
   case lgUtil:checkHwm(Shaper, Event) of
      {true, _Drop, NewShaper} ->
         evalGl(Event, State#state{shaper = NewShaper});
      {drop, Drop, NewShaper} ->
         case Drop =< 0 of
            true ->
               {ok, State#state{shaper = NewShaper}};
            _ ->
               ?LOGFMT(Sink, ?llvWarning, self(), <<"lgErrLoggerH dropped ~p messages in the last second that exceeded the limit of ~p messages/sec">>, [Drop, NewShaper#lgShaper.hwm]),
               evalGl(Event, State#state{shaper = NewShaper})
         end;
      {false, _, NewShaper} ->
         {ok, State#state{shaper = NewShaper}}
   end.

handle_info({mShaperExpired, ?MODULE}, #state{sink = Sink, shaper = Shaper} = State) ->
   case Shaper#lgShaper.dropped of
      0 ->
         ignore;
      Dropped ->
         ?LOGFMT(Sink, ?llvWarning, self(), <<"lgErrLoggerH dropped ~p messages in the last second that exceeded the limit of ~p messages/sec">>, [Dropped, Shaper#lgShaper.hwm])
   end,
   {ok, State#state{shaper = Shaper#lgShaper{dropped = 0}}};
handle_info(_Info, State) ->
   {ok, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, {state, Shaper, GLStrategy}, _Extra) ->
   Raw = lgUtil:get_env(errLoggerFmtRaw, false),
   {ok, #state{
      sink = configSink(),
      shaper = Shaper,
      groupleader_strategy = GLStrategy,
      raw = Raw
   }};
code_change(_OldVsn, {state, Sink, Shaper, GLS}, _Extra) ->
   Raw = lgUtil:get_env(errLoggerFmtRaw, false),
   {ok, #state{sink = Sink, shaper = Shaper, groupleader_strategy = GLS, raw = Raw}};
code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

configSink() ->
   case lgUtil:get_opt(?LgErrLogSink, lgUtil:get_env(extraSinks, []), undefined) of
      undefined -> ?LgDefSink;
      _ -> ?LgErrLogSink
   end.

evalGl(Event, #state{groupleader_strategy = GlStrategy0} = State) ->
   GL = element(2, Event),
   case is_pid(GL) andalso node(GL) =/= node() of
      true ->
         case GlStrategy0 of
            ignore ->
               gen_event:notify({error_logger, node(GL)}, Event),
               {ok, State};
            mirror ->
               gen_event:notify({error_logger, node(GL)}, Event),
               logEvent(Event, State);
            _ ->
               logEvent(Event, State)
         end;
      _ ->
         logEvent(Event, State)
   end.

logEvent(Event, #state{sink = Sink, raw = FormatRaw} = State) ->
   DidLog =
      case Event of
         {error, _GL, {Pid, Fmt, Args}} ->
            case FormatRaw of
               false ->
                  case Fmt of
                     "** Generic server " ++ _ ->
                        %% gen_server terminate
                        {Reason, Name} =
                           case Args of
                              [N, _Msg, _State, R] ->
                                 {R, N};
                              [N, _Msg, _State, R, _Client] ->
                                 %% OTP 20 crash reports where the client pid is dead don't include the stacktrace
                                 {R, N};
                              [N, _Msg, _State, R, _Client, _Stacktrace] ->
                                 %% OTP 20 crash reports contain the pid of the client and stacktrace
                                 %% TODO do something with them
                                 {R, N}
                           end,
                        ?CRASH_LOG(Event),
                        {Md, Formatted} = formatReasonMd(Reason),
                        ?LOGFMT(Sink, ?llvError, [{pid, Pid}, {name, Name} | Md], <<"gen_server ~w terminated with reason: ~s">>, [Name, Formatted]);
                     "** gen_ipc State machine " ++ _ ->
                        %% gen_server terminate
                        {Reason, Name} =
                           case Args of
                              [N, _Msg, _State, R] ->
                                 {R, N};
                              [N, _Msg, _State, R, _Client] ->
                                 %% OTP 20 crash reports where the client pid is dead don't include the stacktrace
                                 {R, N};
                              [N, _Msg, _State, R, _Client, _Stacktrace] ->
                                 %% OTP 20 crash reports contain the pid of the client and stacktrace
                                 %% TODO do something with them
                                 {R, N}
                           end,
                        ?CRASH_LOG(Event),
                        {Md, Formatted} = formatReasonMd(Reason),
                        ?LOGFMT(Sink, ?llvError, [{pid, Pid}, {name, Name} | Md], <<"gen_ipc ~w terminated with reason: ~s">>, [Name, Formatted]);
                     "** State machine " ++ _ ->
                        %% Check if the terminated process is gen_fsm or gen_statem
                        %% since they generate the same exit message
                        {Type, Name, StateName, Reason} =
                           case Args of
                              [TName, _Msg, TStateName, _StateData, TReason] ->
                                 {gen_fsm, TName, TStateName, TReason};
                              %% Handle changed logging in gen_fsm stdlib-3.9 (TPid, ClientArgs)
                              [TName, _Msg, TPid, TStateName, _StateData, TReason | _ClientArgs] when is_pid(TPid), is_atom(TStateName) ->
                                 {gen_fsm, TName, TStateName, TReason};
                              %% Handle changed logging in gen_statem stdlib-3.9 (ClientArgs)
                              [TName, _Msg, {TStateName, _StateData}, _ExitType, TReason, _CallbackMode, Stacktrace | _ClientArgs] ->
                                 {gen_statem, TName, TStateName, {TReason, Stacktrace}};
                              %% Handle changed logging in gen_statem stdlib-3.9 (ClientArgs)
                              [TName, {TStateName, _StateData}, _ExitType, TReason, _CallbackMode, Stacktrace | _ClientArgs] ->
                                 {gen_statem, TName, TStateName, {TReason, Stacktrace}};
                              [TName, _Msg, [{TStateName, _StateData}], _ExitType, TReason, _CallbackMode, Stacktrace | _ClientArgs] ->
                                 %% sometimes gen_statem wraps its statename/data in a list for some reason???
                                 {gen_statem, TName, TStateName, {TReason, Stacktrace}}
                           end,
                        {Md, Formatted} = formatReasonMd(Reason),
                        ?CRASH_LOG(Event),
                        ?LOGFMT(Sink, ?llvError, [{pid, Pid}, {name, Name} | Md], <<"~s ~w in state ~w terminated with reason: ~s">>, [Type, Name, StateName, Formatted]);
                     "** gen_event handler" ++ _ ->
                        %% gen_event handler terminate
                        [ID, Name, _Msg, _State, Reason] = Args,
                        {Md, Formatted} = formatReasonMd(Reason),
                        ?CRASH_LOG(Event),
                        ?LOGFMT(Sink, ?llvError, [{pid, Pid}, {name, Name} | Md], <<"gen_event ~w installed in ~w terminated with reason: ~s">>, [ID, Name, Formatted]);
                     "** Cowboy handler" ++ _ ->
                        %% Cowboy HTTP server error
                        ?CRASH_LOG(Event),
                        case Args of
                           [Module, Function, Arity, _Request, _State] ->
                              %% we only get the 5-element list when its a non-exported function
                              ?LOGFMT(Sink, ?llvError, Pid,
                                 <<"Cowboy handler ~p terminated with reason: call to undefined function ~p:~p/~p">>,
                                 [Module, Module, Function, Arity]);
                           [Module, Function, Arity, _Class, Reason | Tail] ->
                              %% any other cowboy error_format list *always* ends with the stacktrace
                              StackTrace = lists:last(Tail),
                              {Md, Formatted} = formatReasonMd({Reason, StackTrace}),
                              ?LOGFMT(Sink, ?llvError, [{pid, Pid} | Md],
                                 <<"Cowboy handler ~p terminated in ~p:~p/~p with reason: ~s">>, [Module, Module, Function, Arity, Formatted])
                        end;
                     "Ranch listener " ++ _ ->
                        %% Ranch errors
                        ?CRASH_LOG(Event),
                        case Args of
                           %% Error logged by cowboy, which starts as ranch error
                           [Ref, ConnectionPid, StreamID, RequestPid, Reason, StackTrace] ->
                              {Md, Formatted} = formatReasonMd({Reason, StackTrace}),
                              ?LOGFMT(Sink, ?llvError, [{pid, RequestPid} | Md],
                                 <<"Cowboy stream ~p with ranch listener ~p and connection process ~p "
                                 "had its request process exit with reason: ~s">>, [StreamID, Ref, ConnectionPid, Formatted]);
                           [Ref, _Protocol, Worker, {[{reason, Reason}, {mfa, {Module, Function, Arity}}, {stacktrace, StackTrace} | _], _}] ->
                              {Md, Formatted} = formatReasonMd({Reason, StackTrace}),
                              ?LOGFMT(Sink, ?llvError, [{pid, Worker} | Md], <<"Ranch listener ~p terminated in ~p:~p/~p with reason: ~s">>, [Ref, Module, Function, Arity, Formatted]);
                           [Ref, _Protocol, Worker, Reason] ->
                              {Md, Formatted} = formatReasonMd(Reason),
                              ?LOGFMT(Sink, ?llvError, [{pid, Worker} | Md], <<"Ranch listener ~p terminated with reason: ~s">>, [Ref, Formatted]);
                           [Ref, Protocol, Ret] ->
                              %% ranch_conns_sup.erl module line 119-123 has three parameters error msg, log it.
                              {Md, Formatted} = formatReasonMd(Ret),
                              ?LOGFMT(Sink, ?llvError, [{pid, Protocol} | Md], <<"Ranch listener ~p terminated with result:~s">>, [Ref, Formatted])
                        end;
                     "webmachine error" ++ _ ->
                        %% Webmachine HTTP server error
                        ?CRASH_LOG(Event),
                        [Path, Error] = Args,
                        %% webmachine likes to mangle the stack, for some reason
                        StackTrace =
                           case Error of
                              {error, {error, Reason, Stack}} ->
                                 {Reason, Stack};
                              _ ->
                                 Error
                           end,
                        {Md, Formatted} = formatReasonMd(StackTrace),
                        ?LOGFMT(Sink, ?llvError, [{pid, Pid} | Md], <<"Webmachine error at path ~p : ~s">>, [Path, Formatted]);
                     _ ->
                        ?CRASH_LOG(Event),
                        ?LOGFMT(Sink, ?llvError, Pid, Fmt, Args)
                  end;
               _ ->
                  ?CRASH_LOG(Event),
                  ?LOGFMT(Sink, ?llvError, Pid, Fmt, Args)
            end;
         {error_report, _GL, {Pid, std_error, D}} ->
            ?CRASH_LOG(Event),
            ?LOGMSG(Sink, ?llvError, Pid, printSillyList(D));
         {error_report, _GL, {Pid, supervisor_report, D}} ->
            ?CRASH_LOG(Event),
            case lists:sort(D) of
               [{errorContext, Ctx}, {offender, Off}, {reason, Reason}, {supervisor, Name}] ->
                  Offender = formatOffender(Off),
                  {Md, Formatted} = formatReasonMd(Reason),
                  ?LOGFMT(Sink, ?llvError, [{pid, Pid} | Md],
                     <<"Supervisor ~w had child ~s exit with reason ~s in context ~w">>,
                     [supervisorName(Name), Offender, Formatted, Ctx]);
               _ ->
                  ?LOGMSG(Sink, ?llvError, Pid, <<"SUPERVISOR REPORT ", (printSillyList(D))/binary>>)
            end;
         {error_report, _GL, {Pid, crash_report, [Self, Neighbours]}} ->
            ?CRASH_LOG(Event),
            {Md, Formatted} = formatCrashReport(Self, Neighbours),
            ?LOGMSG(Sink, ?llvError, [{pid, Pid} | Md], <<"CRASH REPORT ", Formatted/binary>>);
         {warning_msg, _GL, {Pid, Fmt, Args}} ->
            ?LOGFMT(Sink, ?llvWarning, Pid, Fmt, Args);
         {warning_report, _GL, {Pid, std_warning, Report}} ->
            ?LOGMSG(Sink, ?llvWarning, Pid, printSillyList(Report));
         {info_msg, _GL, {Pid, Fmt, Args}} ->
            ?LOGFMT(Sink, ?llvInfo, Pid, Fmt, Args);
         {info_report, _GL, {Pid, std_info, D}} when is_list(D) ->
            Details = lists:sort(D),
            case Details of
               [{application, App}, {exited, Reason}, {type, _Type}] ->
                  case lgUtil:get_env(suppressAppStartStop, false) of
                     true when Reason == stopped ->
                        no_log;
                     _ ->
                        {Md, Formatted} = formatReasonMd(Reason),
                        ?LOGFMT(Sink, ?llvInfo, [{pid, Pid} | Md], <<"Application ~w exited with reason: ~s">>, [App, Formatted])
                  end;
               _ ->
                  ?LOGMSG(Sink, ?llvInfo, Pid, printSillyList(D))
            end;
         {info_report, _GL, {Pid, std_info, D}} ->
            ?LOGFMT(Sink, ?llvInfo, Pid, "~w", [D]);
         {info_report, _GL, {P, progress, D}} ->
            Details = lists:sort(D),
            case Details of
               [{application, App}, {started_at, Node}] ->
                  case lgUtil:get_env(suppressAppStartStop, false) of
                     true ->
                        no_log;
                     _ ->
                        ?LOGFMT(Sink, ?llvInfo, P, <<"Application ~w started on node ~w">>, [App, Node])
                  end;
               [{started, Started}, {supervisor, Name}] ->
                  case lgUtil:get_env(suppressSupStartStop, false) of
                     true ->
                        no_log;
                     _ ->
                        MFA = formatMfa(get_value(mfargs, Started)),
                        Pid = get_value(pid, Started),
                        ?LOGFMT(Sink, ?llvDebug, P, <<"Supervisor ~w started ~s at pid ~w">>,
                           [supervisorName(Name), MFA, Pid])
                  end;
               _ ->
                  ?LOGMSG(Sink, ?llvInfo, P, <<"PROGRESS REPORT ", (printSillyList(D))/binary>>)
            end;
         _ ->
            ?LOGFMT(Sink, ?llvWarning, self(), <<"Unexpected error_logger event ~w">>, [Event])
      end,
   case DidLog of
      logged ->
         {ok, State};
      no_log ->
         Shaper = State#state.shaper,
         {ok, State#state{shaper = Shaper#lgShaper{mps = Shaper#lgShaper.mps - 1}}}
   end.

formatCrashReport(Report, Neighbours) ->
   Name =
      case get_value(registered_name, Report, []) of
         [] ->
            %% process_info(Pid, registered_name) returns [] for unregistered processes
            get_value(pid, Report);
         Atom -> Atom
      end,
   Md0 =
      case get_value(dictionary, Report, []) of
         [] ->
            %% process_info(Pid, registered_name) returns [] for unregistered processes
            [];
         Dict ->
            %% pull the eLog metadata out of the process dictionary, if we can
            get_value(?PdMdKey, Dict, [])
      end,

   {Class, Reason, Trace} = get_value(error_info, Report),
   {Md, ReasonStr} = formatReasonMd({Reason, Trace}),
   Type = ?lgCASE(Class == exit, <<"exited">>, <<"crashed">>),
   {Md0 ++ Md, eFmt:format(<<"Process ~w with ~w neighbours ~s with reason: ~s">>, [Name, length(Neighbours), Type, ReasonStr])}.

formatOffender(Off) ->
   case get_value(mfargs, Off) of
      undefined ->
         %% supervisor_bridge
         eFmt:format(<<"at module ~w at ~w">>, [get_value(mod, Off), get_value(pid, Off)]);
      MFArgs ->
         %% regular supervisor
         {_, MFA} = formatMfaMd(MFArgs),

         %% In 2014 the error report changed from `name' to
         %% `id', so try that first.
         Name =
            case get_value(id, Off) of
               undefined ->
                  get_value(name, Off);
               Id ->
                  Id
            end,
         eFmt:format(<<"Name ~w at module ~s at ~w">>, [Name, MFA, get_value(pid, Off)])
   end.

%% backwards compatability shim
formatReason(Reason) ->
   element(2, formatReasonMd(Reason)).

-spec formatReasonMd(Reason :: any()) -> {Metadata :: [{atom(), any()}], String :: list()}.
formatReasonMd({'function not exported', [{M, F, A}, MFA | _]}) ->
   {Md, Formatted} = formatMfaMd(MFA),
   {_, Formatted2} = formatMfaMd({M, F, length(A)}),
   {[{reason, 'function not exported'} | Md], <<"call to undefined function ", Formatted2/binary, " from ", Formatted/binary>>};
formatReasonMd({'function not exported', [{M, F, A, _Props}, MFA | _]}) ->
   %% R15 line numbers
   {Md, Formatted} = formatMfaMd(MFA),
   {_, Formatted2} = formatMfaMd({M, F, length(A)}),
   {[{reason, 'function not exported'} | Md], <<"call to undefined function ", Formatted2/binary, " from ", Formatted/binary>>};
formatReasonMd({undef, [MFA | _]}) ->
   {Md, Formatted} = formatMfaMd(MFA),
   {[{reason, undef} | Md], <<"call to undefined function ", Formatted/binary>>};
formatReasonMd({bad_return, {_MFA, {'EXIT', Reason}}}) ->
   formatReasonMd(Reason);
formatReasonMd({bad_return, {MFA, Val}}) ->
   {Md, Formatted} = formatMfaMd(MFA),
   {[{reason, bad_return} | Md], <<"bad return value ", (formatVal(Val))/binary, " from ", Formatted/binary>>};
formatReasonMd({bad_return_value, Val}) ->
   {[{reason, bad_return}], <<"bad return value: ", (formatVal(Val))/binary>>};
formatReasonMd({{bad_return_value, Val}, MFA}) ->
   {Md, Formatted} = formatMfaMd(MFA),
   {[{reason, bad_return_value} | Md], <<"bad return value: ", (formatVal(Val))/binary, " in ", Formatted/binary>>};
formatReasonMd({{badrecord, Record}, [MFA | _]}) ->
   {Md, Formatted} = formatMfaMd(MFA),
   {[{reason, badrecord} | Md], <<"bad record ", (formatVal(Record))/binary, " in ", Formatted/binary>>};
formatReasonMd({{case_clause, Val}, [MFA | _]}) ->
   {Md, Formatted} = formatMfaMd(MFA),
   {[{reason, case_clause} | Md], <<"no case clause matching ", (formatVal(Val))/binary, " in ", Formatted/binary>>};
formatReasonMd({function_clause, [MFA | _]}) ->
   {Md, Formatted} = formatMfaMd(MFA),
   {[{reason, function_clause} | Md], <<"no function clause matching ", Formatted/binary>>};
formatReasonMd({if_clause, [MFA | _]}) ->
   {Md, Formatted} = formatMfaMd(MFA),
   {[{reason, if_clause} | Md], <<"no true branch found while evaluating if expression in ", Formatted/binary>>};
formatReasonMd({{try_clause, Val}, [MFA | _]}) ->
   {Md, Formatted} = formatMfaMd(MFA),
   {[{reason, try_clause} | Md], <<"no try clause matching ", (formatVal(Val))/binary, " in ", Formatted/binary>>};
formatReasonMd({badarith, [MFA | _]}) ->
   {Md, Formatted} = formatMfaMd(MFA),
   {[{reason, badarith} | Md], <<"bad arithmetic expression in ", Formatted/binary>>};
formatReasonMd({{badmatch, Val}, [MFA | _]}) ->
   {Md, Formatted} = formatMfaMd(MFA),
   {[{reason, badmatch} | Md], <<"no match of right hand value ", (formatVal(Val))/binary, " in ", Formatted/binary>>};
formatReasonMd({emfile, _Trace}) ->
   {[{reason, emfile}], <<"maximum number of file descriptors exhausted, check ulimit -n">>};
formatReasonMd({system_limit, [{M, F, _} | _] = Trace}) ->
   Limit =
      case {M, F} of
         {erlang, open_port} ->
            <<"maximum number of ports exceeded">>;
         {erlang, spawn} ->
            <<"maximum number of processes exceeded">>;
         {erlang, spawn_opt} ->
            <<"maximum number of processes exceeded">>;
         {erlang, list_to_atom} ->
            <<"tried to create an atom larger than 255, or maximum atom count exceeded">>;
         {ets, new} ->
            <<"maximum number of ETS tables exceeded">>;
         _ ->
            eFmt:format(<<"~p">>, [Trace], [{charsLimit, 500}])
      end,
   {[{reason, system_limit}], <<"system limit: ", Limit/binary>>};
formatReasonMd({badarg, [MFA, MFA2 | _]}) ->
   case MFA of
      {_M, _F, A, _Props} when is_list(A) ->
         %% R15 line numbers
         {Md, Formatted} = formatMfaMd(MFA2),
         {_, Formatted2} = formatMfaMd(MFA),
         {[{reason, badarg} | Md], <<"bad argument in call to ", Formatted2/binary, " in ", Formatted/binary>>};
      {_M, _F, A} when is_list(A) ->
         {Md, Formatted} = formatMfaMd(MFA2),
         {_, Formatted2} = formatMfaMd(MFA),
         {[{reason, badarg} | Md], <<"bad argument in call to ", Formatted2/binary, " in ", Formatted/binary>>};
      _ ->
         %% seems to be generated by a bad call to a BIF
         {Md, Formatted} = formatMfaMd(MFA),
         {[{reason, badarg} | Md], <<"bad argument in ", Formatted/binary>>}
   end;
formatReasonMd({{badarg, Stack}, _}) ->
   formatReasonMd({badarg, Stack});
formatReasonMd({{badarity, {Fun, Args}}, [MFA | _]}) ->
   {arity, Arity} = lists:keyfind(arity, 1, erlang:fun_info(Fun)),
   {Md, Formatted} = formatMfaMd(MFA),
   {[{reason, badarity} | Md], eFmt:format(<<"fun called with wrong arity of ~w instead of ~w in ~s">>, [length(Args), Arity, Formatted])};
formatReasonMd({noproc, MFA}) ->
   {Md, Formatted} = formatMfaMd(MFA),
   {[{reason, noproc} | Md], <<"no such process or port in call to ", Formatted/binary>>};
formatReasonMd({{badfun, Term}, [MFA | _]}) ->
   {Md, Formatted} = formatMfaMd(MFA),
   {[{reason, badfun} | Md], <<"bad function ", (formatVal(Term))/binary, " in ", Formatted/binary>>};
formatReasonMd({Reason, [{M, F, A} | _]}) when is_atom(M), is_atom(F), is_integer(A) ->
   {Md, Formatted} = formatReasonMd(Reason),
   {_, Formatted2} = formatMfaMd({M, F, A}),
   {Md, <<Formatted/binary, " in ", Formatted2/binary>>};
formatReasonMd({Reason, [{M, F, A, Props} | _]}) when is_atom(M), is_atom(F), is_integer(A), is_list(Props) ->
   %% line numbers
   {Md, Formatted} = formatReasonMd(Reason),
   {_, Formatted2} = formatMfaMd({M, F, A, Props}),
   {Md, <<Formatted/binary, " in ", Formatted2/binary>>};
formatReasonMd(Reason) ->
   {[], eFmt:format(<<"~p">>, [Reason], [{charsLimit, 500}])}.

%% backwards compatability shim
formatMfa(MFA) ->
   element(2, formatMfaMd(MFA)).

-spec formatMfaMd(any()) -> {[{atom(), any()}], list()}.
formatMfaMd({M, F, A}) when is_list(A) ->
   ArgsStr = formatArgs(A, <<>>),
   {[{module, M}, {function, F}], eFmt:format(<<"~w:~w(", ArgsStr/binary, ")">>, [M, F])};
formatMfaMd({M, F, A}) when is_integer(A) ->
   {[{module, M}, {function, F}], eFmt:format(<<"~w:~w/~w">>, [M, F, A])};
formatMfaMd({M, F, A, Props}) when is_list(Props) ->
   case get_value(line, Props) of
      undefined ->
         formatMfaMd({M, F, A});
      Line ->
         {Md, Formatted} = formatMfaMd({M, F, A}),
         {[{line, Line} | Md], <<Formatted/binary, " line ", (integer_to_binary(Line))/binary>>}
   end;
formatMfaMd([{M, F, A} | _]) ->
   %% this kind of weird stacktrace can be generated by a uncaught throw in a gen_server
   formatMfaMd({M, F, A});
formatMfaMd([{M, F, A, Props} | _]) when is_list(Props) ->
   %% this kind of weird stacktrace can be generated by a uncaught throw in a gen_server
   %% TODO we might not always want to print the first MFA we see here, often it is more helpful
   %% to print a lower one, but it is hard to programatically decide.
   formatMfaMd({M, F, A, Props});
formatMfaMd(Other) ->
   {[], eFmt:format(<<"~w">>, [Other])}.

formatArgs([], ArgsAcc) ->
   ArgsAcc;
formatArgs([H], ArgsAcc) ->
   Str = eFmt:format(<<"~p">>, [H], [{charsLimit, 100}]),
   <<ArgsAcc/binary, Str/binary>>;
formatArgs([H | T], ArgsAcc) ->
   Str = eFmt:format(<<"~p">>, [H], [{charsLimit, 100}]),
   formatArgs(T, <<ArgsAcc/binary, Str/binary, ",">>).

formatVal(Val) ->
   eFmt:format(<<"~p">>, [Val], [{charsLimit, 500}]).

printSillyList(L) ->
   eFmt:format(<<"~p">>, [L], [{charsLimit, ?LgDefTruncation}]).

%% @doc Faster than proplists, but with the same API as long as you don't need to
%% handle bare atom keys
get_value(Key, Value) ->
   get_value(Key, Value, undefined).

get_value(Key, List, Default) ->
   case lists:keyfind(Key, 1, List) of
      false -> Default;
      {Key, Value} -> Value
   end.

supervisorName({local, Name}) -> Name;
supervisorName(Name) -> Name.

shaperFun() ->
   SupSS = lgUtil:get_env(suppressSupStartStop, false),
   AppSS = lgUtil:get_env(suppressAppStartStop, false),
   if
      SupSS andalso AppSS ->
         fun ?MODULE:suppress_supervisor_start_and_application_start/1;
      SupSS ->
         fun ?MODULE:suppress_supervisor_start/1;
      AppSS ->
         fun ?MODULE:suppress_application_start/1;
      true ->
         undefined
   end.

suppress_supervisor_start_and_application_start(E) ->
   suppress_supervisor_start(E) orelse suppress_application_start(E).

suppress_application_start({info_report, _GL, {_Pid, std_info, D}}) when is_list(D) ->
   lists:member({exited, stopped}, D);
suppress_application_start({info_report, _GL, {_P, progress, D}}) ->
   lists:keymember(application, 1, D) andalso lists:keymember(started_at, 1, D);
suppress_application_start(_) ->
   false.

suppress_supervisor_start({info_report, _GL, {_P, progress, D}}) ->
   lists:keymember(started, 1, D) andalso lists:keymember(supervisor, 1, D);
suppress_supervisor_start(_) ->
   false.
