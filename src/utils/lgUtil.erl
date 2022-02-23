-module(lgUtil).

-include("lgCom.hrl").
-include("lgDef.hrl").
-include_lib("kernel/include/file.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   levels/0
   , levelToNum/1
   , levelToChr/1
   , numToLevel/1
   , validateLogLevel/1
   , configToMask/1
   , atomCfgToLevels/1
   , maskToLevels/1
   , nowMs/0
   , msToBinStr/0
   , msToBinStr/1
   , curYMDHMStr/0
   , parseRotateSpec/1
   , calcNextRotateMs/1
   , calcNextRotateMs/2
   , calcNextRotateDt/2
   , checkHwm/1
   , checkHwm/2
   , makeInnerSinkName/1
   , isFileChanged/3
   , isErrorReport/1
   , get_env/2
   , get_opt/3
   , sup_get/2
   , isLoggAble/3
   , parsePath/1
   , validate_trace/1
   , check_traces/4
   , trace_filter/1
   , trace_filter/2
   , find_file/2
]).

levels() ->
   [debug, info, notice, warning, error, critical, alert, emergency, none].

levelToNum(debug) -> ?llvDebug;
levelToNum(info) -> ?llvInfo;
levelToNum(notice) -> ?llvNotice;
levelToNum(warning) -> ?llvWarning;
levelToNum(error) -> ?llvError;
levelToNum(critical) -> ?llvCritical;
levelToNum(alert) -> ?llvAlert;
levelToNum(emergency) -> ?llvEmergency;
levelToNum(none) -> ?llvNone.

numToLevel(?llvDebug) -> debug;
numToLevel(?llvInfo) -> info;
numToLevel(?llvNotice) -> notice;
numToLevel(?llvWarning) -> warning;
numToLevel(?llvError) -> error;
numToLevel(?llvCritical) -> critical;
numToLevel(?llvAlert) -> alert;
numToLevel(?llvEmergency) -> emergency;
numToLevel(?llvNone) -> none.

levelToChr(debug) -> $D;
levelToChr(info) -> $I;
levelToChr(notice) -> $N;
levelToChr(warning) -> $W;
levelToChr(error) -> $E;
levelToChr(critical) -> $C;
levelToChr(alert) -> $A;
levelToChr(emergency) -> $M;
levelToChr(none) -> $ .

-spec validateLogLevel(atom()|string()) -> false | lgMaskLevel().
validateLogLevel(Level) ->
   try lgUtil:configToMask(Level) of
      Levels ->
         Levels
   catch
      _:_ ->
         false
   end.

-spec configToMask(atom()|string()) -> lgMaskLevel().
configToMask(Conf) ->
   Levels = atomCfgToLevels(Conf),
   levelsToMask(Levels, 0).

-spec levelsToMask([lgAtomLevel()], lgMaskLevel()) -> lgMaskLevel().
levelsToMask([], Acc) ->
   Acc;
levelsToMask([Level | Left], Acc) ->
   levelsToMask(Left, levelToNum(Level) bor Acc).

-spec maskToLevels(lgMaskLevel()) -> [lgAtomLevel()].
maskToLevels(Mask) ->
   maskToLevels(?LgLevels, Mask, []).

maskToLevels([], _Mask, Acc) ->
   lists:reverse(Acc);
maskToLevels([Level | Levels], Mask, Acc) ->
   case (levelToNum(Level) band Mask) =/= 0 of
      true ->
         maskToLevels(Levels, Mask, [Level | Acc]);
      _ ->
         maskToLevels(Levels, Mask, Acc)
   end.

-spec atomCfgToLevels(atom()) -> [lgAtomLevel()].
atomCfgToLevels(Cfg) ->
   binCfgToLevels(atom_to_binary(Cfg, utf8)).

binCfgToLevels(<<"!", Rest/binary>>) ->
   ?LgLevels -- binCfgToLevels(Rest);
binCfgToLevels(<<"=<", Rest/binary>>) ->
   riseInWhile(?LgLevels, levelBinToAtom(Rest), []);
binCfgToLevels(<<"<=", Rest/binary>>) ->
   riseInWhile(?LgLevels, levelBinToAtom(Rest), []);
binCfgToLevels(<<">=", Rest/binary>>) ->
   dropInWhile(?LgLevels, levelBinToAtom(Rest));
binCfgToLevels(<<"=>", Rest/binary>>) ->
   dropInWhile(?LgLevels, levelBinToAtom(Rest));
binCfgToLevels(<<"=", Rest/binary>>) ->
   [levelBinToAtom(Rest)];
binCfgToLevels(<<"<", Rest/binary>>) ->
   riseOutWhile(?LgLevels, levelBinToAtom(Rest), []);
binCfgToLevels(<<">", Rest/binary>>) ->
   dropOutWhile(?LgLevels, levelBinToAtom(Rest));
binCfgToLevels(Rest) ->
   [levelBinToAtom(Rest)].

dropInWhile([], _Level) ->
   [];
dropInWhile([CurLevel | Left] = Rest, Level) ->
   case CurLevel == Level of
      true ->
         Rest;
      _ ->
         dropInWhile(Left, Level)
   end.

dropOutWhile([], _Level) ->
   [];
dropOutWhile([CurLevel | Left], Level) ->
   case CurLevel == Level of
      true ->
         Left;
      _ ->
         dropOutWhile(Left, Level)
   end.

riseInWhile([], _Level, Acc) ->
   Acc;
riseInWhile([CurLevel | Left], Level, Acc) ->
   case CurLevel == Level of
      true ->
         [CurLevel | Acc];
      _ ->
         riseInWhile(Left, Level, [CurLevel | Acc])
   end.

riseOutWhile([], _Level, Acc) ->
   Acc;
riseOutWhile([CurLevel | Left], Level, Acc) ->
   case CurLevel == Level of
      true ->
         Acc;
      _ ->
         riseOutWhile(Left, Level, [CurLevel | Acc])
   end.

levelBinToAtom(BinStr) ->
   AtomLevel = binary_to_atom(BinStr, utf8),
   case lists:member(AtomLevel, ?LgLevels) of
      true ->
         AtomLevel;
      _ ->
         erlang:error(badarg)
   end.

nowMs() ->
   erlang:system_time(millisecond).

msToBinStr() ->
   msToBinStr(nowMs()).

msToBinStr(MsTick) ->
   ThisSec = MsTick div 1000,
   ThisMs = MsTick rem 1000,
   {{Y, M, D}, {H, Mi, S}} = erlang:universaltime_to_localtime(erlang:posixtime_to_universaltime(ThisSec)),
   <<(integer_to_binary(Y))/binary, "-", (i2b(M))/binary, "-", (i2b(D))/binary, " ", (i2b(H))/binary, ":", (i2b(Mi))/binary, ":", (i2b(S))/binary, ".", (i3b(ThisMs))/binary>>.

curYMDHMStr() ->
   {{Y, M, D}, {H, Mi, _S}} = lgTime:curDateTime(),
   <<(integer_to_binary(Y))/binary, (i2b(M))/binary, (i2b(D))/binary, (i2b(H))/binary, (i2b(Mi))/binary>>.

i2b(Num) ->
   if
      Num < 10 ->
         <<"0", (integer_to_binary(Num))/binary>>;
      true ->
         integer_to_binary(Num)
   end.

i3b(Num) ->
   if
      Num < 10 ->
         <<"00", (integer_to_binary(Num))/binary>>;
      Num < 100 ->
         <<"0", (integer_to_binary(Num))/binary>>;
      true ->
         integer_to_binary(Num)
   end.

%% last parse hour
parseRotateHourSpec([], DayOrMonthF, Hour, Minute, DayOrMonthV) ->
   {DayOrMonthF, Hour, Minute, DayOrMonthV};
parseRotateHourSpec([$H, M1, M2], DayOrMonthF, Hour, _Minute, DayOrMonthV) when M1 >= $0, M1 =< $9, M2 >= $0, M2 =< $9 ->
   Min = list_to_integer([M1, M2]),
   ?IIF(Min >= 0 andalso Min =< 59, {DayOrMonthF, Hour, Min, DayOrMonthV}, {error, invalid_date_spec});
parseRotateHourSpec([$H, M], DayOrMonthF, Hour, _Minute, DayOrMonthV) when M >= $0, M =< $9 ->
   {DayOrMonthF, Hour, M - $0, DayOrMonthV};
parseRotateHourSpec(_, _DayOrMonth, _Hour, _Minute, _DayOrMonthV) ->
   {error, invalid_date_spec}.

%% second parse day Default to 00:00:00 rotation
parseRotateDaySpec([], DayOrMonthF, Hour, Minute, DayOrMonthV) ->
   {DayOrMonthF, Hour, Minute, DayOrMonthV};
parseRotateDaySpec([$D, D1, D2 | T], DayOrMonthF, _Hour, _Minute, DayOrMonthV) when D1 > $0, D1 < $9, D2 > $0, D2 < $9 ->
   Day = list_to_integer([D1, D2]),
   ?IIF(Day >= 0 andalso Day =< 23, parseRotateHourSpec(T, DayOrMonthF, Day, 0, DayOrMonthV), {error, invalid_date_spec});
parseRotateDaySpec([$D, D | T], DayOrMonthF, _Hour, _Minute, DayOrMonthV) when D >= $0, D =< $9 ->
   parseRotateHourSpec(T, DayOrMonthF, D - $0, 0, DayOrMonthV);
parseRotateDaySpec(T, DayOrMonth, Hour, Minute, DayOrMonthV) ->
   parseRotateHourSpec(T, DayOrMonth, Hour, Minute, DayOrMonthV).

%% first parse date or week
parseRotateDateSpec([$$, $W, W | T], _DayOrMonthF, _Hour, _Minute, _DayOrMonthV) when W >= $1, W =< $7 ->
   parseRotateDaySpec(T, day, 0, 0, W - $0);
parseRotateDateSpec([$$, $M, L | T], _DayOrMonthF, _Hour, _Minute, DayOrMonthV) when L == $L; L == $l ->
   parseRotateDaySpec(T, last, 0, 0, DayOrMonthV);
parseRotateDateSpec([$$, $M, M1, M2 | T], _DayOrMonthF, _Hour, _Minute, _DayOrMonthV) when M1 >= $0, M1 =< $9, M2 >= $0, M2 =< $9 ->
   Date = list_to_integer([M1, M2]),
   ?IIF(Date >= 1 andalso Date =< 31, parseRotateDaySpec(T, date, 0, 0, Date), {error, invalid_date_spec});
parseRotateDateSpec([$$, $M, M | T], _DayOrMonthF, _Hour, _Minute, _DayOrMonthV) when M >= $1, M =< $9 ->
   parseRotateDaySpec(T, date, 0, 0, M - $0);
parseRotateDateSpec([$$ | T], DayOrMonthF, Hour, Minute, DayOrMonthV) ->
   parseRotateDaySpec(T, DayOrMonthF, Hour, Minute, DayOrMonthV);
parseRotateDateSpec(_, _DayOrMonthF, _Hour, _Minute, _DayOrMonthV) ->
   {error, invalid_date_spec}.

parseRotateSpec(Spec) ->
   SpecList = ?IIF(is_binary(Spec), binary_to_list(Spec), Spec),
   case parseRotateDateSpec(SpecList, undefined, undefined, undefined, undefined) of
      {error, _} = ErrRet ->
         ErrRet;
      {undefined, undefined, undefined, _} ->
         {error, invalid_date_spec};
      STuple ->
         {ok, STuple}
   end.

calcNextRotateMs(Spec) ->
   {Date, Time} = NowDataTime = erlang:localtime(),
   NextTime = calcNextRotate(Spec, Date, Time),
   (lgTime:lDateTimeToSec(NextTime) - lgTime:lDateTimeToSec(NowDataTime)) * 1000.

calcNextRotateMs(Spec, NowDataTime) ->
   {Date, Time} = NowDataTime,
   NextTime = calcNextRotate(Spec, Date, Time),
   (lgTime:lDateTimeToSec(NextTime) - lgTime:lDateTimeToSec(NowDataTime)) * 1000.

calcNextRotateDt(Spec, NowDataTime) ->
   {Date, Time} = NowDataTime,
   calcNextRotate(Spec, Date, Time).

calcNextRotate({undefined, SHour, SMinute, _SMonthV}, CurDate, CurTime) ->
   case SHour of
      undefined ->
         {CurHour, CurMinute, _} = CurTime,
         case CurMinute < SMinute of
            true ->
               %% rotation is this hour
               {CurDate, {CurHour, SMinute, 0}};
            _ ->
               %% rotation is next hour
               NexSec = lgTime:lDateTimeToSec({CurDate, {CurHour, SMinute, 0}}) + 3600,
               lgTime:secToLDateTime(NexSec)
         end;
      _ ->
         case CurTime < {SHour, SMinute, 0} of
            true ->
               %% rotation is this day
               {CurDate, {SHour, SMinute, 0}};
            _ ->
               %% rotation is next day
               NexSec = lgTime:lDateTimeToSec({CurDate, {SHour, SMinute, 0}}) + 86400,
               lgTime:secToLDateTime(NexSec)
         end
   end;
calcNextRotate({day, SHour, SMinute, SDay}, CurDate, CurTime) ->
   CurWeekDay = lgTime:weekDay(CurDate),
   if
      CurWeekDay < SDay ->
         %% rotation is this week
         DiffDays = SDay - CurWeekDay,
         NexSec = lgTime:lDateTimeToSec({CurDate, {SHour, SMinute, 0}}) + (86400 * DiffDays),
         lgTime:secToLDateTime(NexSec);
      CurWeekDay > SDay ->
         %% rotation is next week
         DiffDays = ((7 - CurWeekDay) + SDay),
         NexSec = lgTime:lDateTimeToSec({CurDate, {SHour, SMinute, 0}}) + (86400 * DiffDays),
         lgTime:secToLDateTime(NexSec);
      true ->
         case CurTime < {SHour, SMinute, 0} of
            true ->
               %% rotation is this week
               {CurDate, {SHour, SMinute, 0}};
            _ ->
               %% rotation is next week
               NexSec = lgTime:lDateTimeToSec({CurDate, {SHour, SMinute, 0}}) + (86400 * 7),
               lgTime:secToLDateTime(NexSec)
         end
   end;
calcNextRotate({last, SHour, SMinute, _SMonthV}, CurDate, CurTime) ->
   {CurYear, CurMonth, CurDay} = CurDate,
   CurMonthDay = lgTime:monthDay(CurYear, CurMonth),
   case CurMonthDay == CurDay of
      true ->
         case CurTime < {SHour, SMinute, 0} of
            true ->
               %% rotation is this last month day
               {CurDate, {SHour, SMinute, 0}};
            _ ->
               %% rotation is next last month day
               NexSec = lgTime:lDateTimeToSec({CurDate, {23, 59, 59}}) + 1,  %% 下个月1号凌晨
               {NewNDate, _NewNTime} = lgTime:secToLDateTime(NexSec),
               {NewNYear, NewNMonth, _} = NewNDate,
               NewMonthDay = lgTime:monthDay(NewNYear, NewNMonth),
               {{NewNYear, NewNMonth, NewMonthDay}, {SHour, SMinute, 0}}
         end;
      _ ->
         %% rotation is this last month day
         {{CurYear, CurMonth, CurMonthDay}, {SHour, SMinute, 0}}
   end;
calcNextRotate({date, SHour, SMinute, SDate}, CurDate, CurTime) ->
   {CurYear, CurMonth, CurDay} = CurDate,
   if
      CurDay < SDate ->
         %% rotation is this month day
         {{CurYear, CurMonth, SDate}, {SHour, SMinute, 0}};
      CurDay > SDate ->
         %% rotation is next month day
         CurMonthDay = lgTime:monthDay(CurYear, CurMonth),
         NexSec = lgTime:lDateTimeToSec({{CurYear, CurMonth, CurMonthDay}, {23, 59, 59}}) + 1,
         {NewNDate, _NewNTime} = lgTime:secToLDateTime(NexSec),
         {NewNYear, NewNMonth, _} = NewNDate,
         {{NewNYear, NewNMonth, SDate}, {SHour, SMinute, 0}};
      true ->
         case CurTime < {SHour, SMinute, 0} of
            true ->
               %% rotation is this month day
               {CurDate, {SHour, SMinute, 0}};
            _ ->
               %% rotation is next month day
               CurMonthDay = lgTime:monthDay(CurYear, CurMonth),
               NexSec = lgTime:lDateTimeToSec({{CurYear, CurMonth, CurMonthDay}, {23, 59, 59}}) + 1,
               {NewNDate, _NewNTime} = lgTime:secToLDateTime(NexSec),
               {NewNYear, NewNMonth, _} = NewNDate,
               {{NewNYear, NewNMonth, SDate}, {SHour, SMinute, 0}}
         end
   end.


%% conditionally check the Hwm if the event would not have been filtered
checkHwm(#lgShaper{filter = Filter} = Shaper, Event) ->
   case Filter =/= undefined andalso Filter(Event) of
      true ->
         {true, 0, Shaper};
      _ ->
         checkHwm(Shaper)
   end.

%% 日志速率限制S i.e. 即传入消息的高水位标记
checkHwm(#lgShaper{id = Id, hwm = Hwm, mps = Mps, lastTime = LastTime, dropped = Drop, flushQueue = FlushQueue, flushThr = FlushThr, timer = Timer, filter = Filter} = Shaper) ->
   if
      Hwm == undefined ->
         {true, 0, Shaper};
      Mps < Hwm ->
         NowTime = lgTime:now(),
         case LastTime == NowTime of
            true ->
               {true, 0, Shaper#lgShaper{mps = Mps + 1}};
            _ ->
               %different second - reset mps
               {true, 0, Shaper#lgShaper{mps = 1, lastTime = NowTime}}
         end;
      true ->
         %% are we still in the same second?
         NowTimeMs = lgTime:nowMs(),
         NowTime = NowTimeMs div 1000,
         case LastTime == NowTime of
            true ->
               PastMs = NowTimeMs rem 1000,
               %% still in same second, but have exceeded the high water mark
               NewDrops = ?IIF(isNeedFlush(FlushQueue, FlushThr), dropMsg(NowTime, Filter, 0), 0),
               NewTimer = ?IIF(erlang:read_timer(Timer) =/= false, Timer, erlang:send_after(1000 - PastMs, self(), {mShaperExpired, Id})),
               {false, 0, Shaper#lgShaper{dropped = Drop + NewDrops + 1, timer = NewTimer}};
            _ ->
               _ = erlang:cancel_timer(Shaper#lgShaper.timer),
               %% different second, reset all counters and allow it
               {drop, Drop, Shaper#lgShaper{mps = 1, lastTime = NowTime}}
         end
   end.

isNeedFlush(true, FlushThreshold) ->
   case FlushThreshold of
      0 ->
         true;
      _ ->
         PInfo = process_info(self(), message_queue_len),
         element(2, PInfo) > FlushThreshold
   end;
isNeedFlush(_FlushQueue, _FlushThreshold) ->
   false.

dropMsg(LastTime, Filter, Count) ->
   CurTime = lgTime:now(),
   case CurTime == LastTime of
      true ->
         receive
         %% we only discard gen_event notifications, because
         %% otherwise we might discard gen_event internal
         %% messages, such as trapped EXITs
            {'$gen_info', Event} ->
               NewCount = ?IIF(Filter(Event), Count, Count + 1),
               dropMsg(LastTime, Filter, NewCount)
         after 0 ->
            Count
         end;
      _ ->
         Count
   end.

%% @private Build an atom for the gen_event process based on a sink name.
%% For historical reasons, the default gen_event process for eLog itself is named
%% `eLogEvent'. For all other sinks, it is SinkName++`_lgEvent'
makeInnerSinkName(Sink) ->
   binary_to_atom(<<(atom_to_binary(Sink, utf8))/binary, "Event">>).

-spec isFileChanged(FileName :: file:name_all(), Inode :: pos_integer(), Ctime :: file:date_time()) -> {boolean(), file:file_info() | undefined}.
isFileChanged(FileName, Inode, Ctime) ->
   case file:read_file_info(FileName, [raw]) of
      {ok, FileInfo} ->
         case os:type() of
            {win32, _} ->
               % Note: on win32, Inode is always zero So check the file's ctime to see if it needs to be re-opened
               {Ctime =/= FileInfo#file_info.ctime, FileInfo};
            _ ->
               {Inode =/= FileInfo#file_info.inode, FileInfo}
         end;
      _ ->
         {true, undefined}
   end.

-spec get_env(Par :: atom(), Def :: term()) -> Val :: term().
get_env(Key, Def) ->
   case application:get_env(?LgAppName, Key) of
      {ok, Val} ->
         Val;
      _ ->
         Def
   end.

get_opt(Key, Opts, Def) ->
   case lists:keyfind(Key, 1, Opts) of
      false ->
         Def;
      V ->
         element(2, V)
   end.

-spec sup_get(term(), [proplists:property()]) -> term().
sup_get(Tag, Report) ->
   case lists:keysearch(Tag, 1, Report) of
      {value, {_, Value}} ->
         Value;
      _ ->
         ""
   end.

%% From OTP sasl's sasl_report.erl ... These functions aren't
%% exported.
-spec isErrorReport(atom()) -> boolean().
isErrorReport(supervisor_report) -> true;
isErrorReport(crash_report) -> true;
isErrorReport(_) -> false.

-spec isLoggAble(lgMsg(), lgMaskLevel(), term()) -> boolean().
isLoggAble(LgMsg, Mask, MyName) ->
   #lgMsg{severity = Severity, destinations = Destinations} = LgMsg,
   (Severity band Mask) =/= 0 orelse lists:member(MyName, Destinations).

parsePath(FBName) ->
   LogRoot = lgUtil:get_env(logRoot, ?LgDefLogRoot),
   TimeFileName = <<(lgUtil:curYMDHMStr())/binary, "_", FBName/binary>>,
   WholeFileName = filename:join(LogRoot, TimeFileName),
   filename:absname(WholeFileName).

-spec trace_filter(Query :: 'none' | [tuple()]) -> {ok, any()}.
trace_filter(Query) ->
   trace_filter(?LgDefTracer, Query).

%% TODO: Support multiple trace modules
%-spec trace_filter(Module :: atom(), Query :: 'none' | [tuple()]) -> {ok, any()}.
trace_filter(Module, Query) when Query == none; Query == [] ->
   {ok, _} = glc:compile(Module, glc:null(false));
trace_filter(Module, Query) when is_list(Query) ->
   {ok, _} = glc:compile(Module, glc_lib:reduce(trace_any(Query))).

validate_trace({Filter, Level, {Destination, ID}}) when is_tuple(Filter); is_list(Filter), is_atom(Level), is_atom(Destination) ->
   case validate_trace({Filter, Level, Destination}) of
      {ok, {F, L, D}} ->
         {ok, {F, L, {D, ID}}};
      Error ->
         Error
   end;
validate_trace({Filter, Level, Destination}) when is_tuple(Filter); is_list(Filter), is_atom(Level), is_atom(Destination) ->
   ValidFilter = validate_trace_filter(Filter),
   try configToMask(Level) of
      _ when not ValidFilter ->
         {error, invalid_trace};
      L when is_list(Filter) ->
         {ok, {trace_all(Filter), L, Destination}};
      L ->
         {ok, {Filter, L, Destination}}
   catch
      _:_ ->
         {error, invalid_level}
   end;
validate_trace(_) ->
   {error, invalid_trace}.

validate_trace_filter(Filter) when is_tuple(Filter), is_atom(element(1, Filter)) =:= false ->
   false;
validate_trace_filter(Filter) when is_list(Filter) ->
   lists:all(fun validate_trace_filter/1, Filter);
validate_trace_filter({Key, '*'}) when is_atom(Key) -> true;
validate_trace_filter({any, L}) when is_list(L) -> lists:all(fun validate_trace_filter/1, L);
validate_trace_filter({all, L}) when is_list(L) -> lists:all(fun validate_trace_filter/1, L);
validate_trace_filter({null, Bool}) when is_boolean(Bool) -> true;
validate_trace_filter({Key, _Value}) when is_atom(Key) -> true;
validate_trace_filter({Key, '=', _Value}) when is_atom(Key) -> true;
validate_trace_filter({Key, '!=', _Value}) when is_atom(Key) -> true;
validate_trace_filter({Key, '<', _Value}) when is_atom(Key) -> true;
validate_trace_filter({Key, '=<', _Value}) when is_atom(Key) -> true;
validate_trace_filter({Key, '>', _Value}) when is_atom(Key) -> true;
validate_trace_filter({Key, '>=', _Value}) when is_atom(Key) -> true;
validate_trace_filter(_) -> false.

trace_all(Query) ->
   glc:all(trace_acc(Query)).

trace_any(Query) ->
   glc:any(Query).

trace_acc(Query) ->
   trace_acc(Query, []).

trace_acc([], Acc) ->
   lists:reverse(Acc);
trace_acc([{any, L} | T], Acc) ->
   trace_acc(T, [glc:any(L) | Acc]);
trace_acc([{all, L} | T], Acc) ->
   trace_acc(T, [glc:all(L) | Acc]);
trace_acc([{null, Bool} | T], Acc) ->
   trace_acc(T, [glc:null(Bool) | Acc]);
trace_acc([{Key, '*'} | T], Acc) ->
   trace_acc(T, [glc:wc(Key) | Acc]);
trace_acc([{Key, '!'} | T], Acc) ->
   trace_acc(T, [glc:nf(Key) | Acc]);
trace_acc([{Key, Val} | T], Acc) ->
   trace_acc(T, [glc:eq(Key, Val) | Acc]);
trace_acc([{Key, '=', Val} | T], Acc) ->
   trace_acc(T, [glc:eq(Key, Val) | Acc]);
trace_acc([{Key, '!=', Val} | T], Acc) ->
   trace_acc(T, [glc:neq(Key, Val) | Acc]);
trace_acc([{Key, '>', Val} | T], Acc) ->
   trace_acc(T, [glc:gt(Key, Val) | Acc]);
trace_acc([{Key, '>=', Val} | T], Acc) ->
   trace_acc(T, [glc:gte(Key, Val) | Acc]);
trace_acc([{Key, '=<', Val} | T], Acc) ->
   trace_acc(T, [glc:lte(Key, Val) | Acc]);
trace_acc([{Key, '<', Val} | T], Acc) ->
   trace_acc(T, [glc:lt(Key, Val) | Acc]).

check_traces(_, _, [], Acc) ->
   lists:flatten(Acc);
check_traces(Attrs, Level, [{_, FilterLevel, _} | Flows], Acc) when (Level band FilterLevel) == 0 ->
   check_traces(Attrs, Level, Flows, Acc);
check_traces(Attrs, Level, [{Filter, _, _} | Flows], Acc) when length(Attrs) < length(Filter) ->
   check_traces(Attrs, Level, Flows, Acc);
check_traces(Attrs, Level, [Flow | Flows], Acc) ->
   check_traces(Attrs, Level, Flows, [check_trace(Attrs, Flow) | Acc]).

check_trace(Attrs, {Filter, _Level, Dest}) when is_list(Filter) ->
   check_trace(Attrs, {trace_all(Filter), _Level, Dest});

check_trace(Attrs, {Filter, _Level, Dest}) when is_tuple(Filter) ->
   Made = gre:make(Attrs, [list]),
   glc:handle(?LgDefTracer, Made),
   Match = glc_lib:matches(Filter, Made),
   case Match of
      true ->
         Dest;
      false ->
         []
   end.

%% Find a file among the already installed handlers.
%%
%% The file is already expanded (i.e. lgUtil:expand_path already added the
%% "logRoot"), but the file paths inside Handlers are not.
find_file(_File1, _Handlers = []) ->
   false;
find_file(File1, [{{lgBkdFile, File2}, _Handler, _Sink} = HandlerInfo | Handlers]) ->
   File1Abs = File1,
   File2Abs = lgUtil:parsePath(File2),
   case File1Abs =:= File2Abs of
      true ->
         % The file inside HandlerInfo is the same as the file we are looking
         % for, so we are done.
         HandlerInfo;
      false ->
         find_file(File1, Handlers)
   end;
find_file(File1, [_HandlerInfo | Handlers]) ->
   find_file(File1, Handlers).