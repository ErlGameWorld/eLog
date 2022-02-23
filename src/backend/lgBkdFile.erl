-module(lgBkdFile).

%% @doc File backend for eLog, with multiple file support.
%% Multiple files are supported, each with the path and the loglevel being configurable.
%% The configuration paramter for this backend is a list of key-value 2-tuples. See the init() function for the available options.
%% This backend supports external and internal log rotation and will re-open handles to files if the inode changes.
%% It will also rotate the files itself if the size of the file exceeds the `size' and keep `count' rotated files.
%% `date' is an alternate rotation trigger, based on time. See the README for documentation.
%% For performance, the file backend does delayed writes, although it will sync at specific log levels, configured via the `syncOn' option.
%% By default the error level or above will trigger a sync.

-behaviour(gen_emm).

-include("lgDef.hrl").
-include_lib("kernel/include/file.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([configToId/1]).

-export([
   init/1
   , handleCall/2
   , handleEvent/2
   , handleInfo/2
   , terminate/2
   , code_change/3
]).

-record(state, {
   fileName :: string(),
   fBName :: string(),
   level :: lgMaskLevel(),
   fd :: file:io_device() | undefined,
   inode :: integer() | undefined,
   cTime :: file:date_time() | undefined,
   flap = false :: boolean(),
   size = 0 :: integer(),
   date :: undefined | string(),
   count = 10 :: integer(),
   rotator = lgRotatorIns :: atom(),
   shaper :: lgShaper(),
   fmtTer :: atom(),
   fmtCfg :: any(),
   syncOn :: integer(),
   checkInt = ?LgDefCheckInt :: non_neg_integer(),   %% 单位毫秒
   syncInt = ?LgDefSyncInt :: non_neg_integer(),
   syncSize = ?LgDefSyncSize :: non_neg_integer(),
   lastCheck = lgTime:nowMs() :: erlang:timestamp(),   %% 单位毫秒
   osType :: atom()
}).

-spec init([lgFileOpt(), ...]) -> {ok, #state{}} | {error, atom()}.
init(Opts) ->
   true = checkOpts(Opts, false),
   FBName = lgUtil:get_opt(file, Opts, undefined),
   CfgLevel = lgUtil:get_opt(level, Opts, ?LgDefLogLevel),
   CfgDate = lgUtil:get_opt(date, Opts, ?LgDefRotateDate),
   Size = lgUtil:get_opt(size, Opts, ?LgDefRotateSize),
   Count = lgUtil:get_opt(count, Opts, ?LgDefRotateCnt),
   Rotator = lgUtil:get_opt(rotator, Opts, ?LgDefRotateMod),
   Hwm = lgUtil:get_opt(hwm, Opts, ?LgDefCheckHwm),
   FlushQueue = lgUtil:get_opt(flushQueue, Opts, ?LgDefFlushQueue),
   FlushThr = lgUtil:get_opt(flushThr, Opts, ?LgDefFlushThr),
   SyncInt = lgUtil:get_opt(syncInt, Opts, ?LgDefSyncInt),
   CfgCheckInt = lgUtil:get_opt(checkInt, Opts, ?LgDefCheckInt),
   SyncSize = lgUtil:get_opt(syncSize, Opts, ?LgDefSyncSize),
   CfgSyncOn = lgUtil:get_opt(syncOn, Opts, ?LgDefSyncLevel),
   FmtTer = lgUtil:get_opt(fmtTer, Opts, ?LgDefFmtTer),
   CfgFmtCfg = lgUtil:get_opt(fmtCfg, Opts, ?LgDefFormatterCfg),

   %% 需要二次转换的配置在这里处理
   Level = lgUtil:configToMask(CfgLevel),
   SyncOn = lgUtil:configToMask(CfgSyncOn),
   CheckInt = ?IIF(CfgCheckInt == always, 0, CfgCheckInt),
   {ok, Date} = lgUtil:parseRotateSpec(CfgDate),
   FileName = lgUtil:parsePath(FBName),
   scheduleRotation(Date, FBName),
   FmtCfg = ?IIF(CfgFmtCfg =/= [], CfgFmtCfg, begin MdWhitelist = lgUtil:get_env(mdWhitelist, []), lgFmtTer:fmtCfg(MdWhitelist) end),

   Shaper = #lgShaper{hwm = Hwm, flushQueue = FlushQueue, flushThr = FlushThr, id = FBName},
   TemState = #state{
      fileName = FileName, fBName = FBName, level = Level, size = Size, date = Date
      , count = Count, rotator = Rotator, shaper = Shaper
      , fmtTer = FmtTer, fmtCfg = FmtCfg
      , syncOn = SyncOn, syncInt = SyncInt
      , syncSize = SyncSize, checkInt = CheckInt
   },

   case Rotator:createLogFile(FileName, {SyncSize, SyncInt}) of
      {ok, Fd, Inode, CTime, _Size} ->
         {ok, TemState#state{fd = Fd, inode = Inode, cTime = CTime}};
      {error, Reason} ->
         ?INT_LOG(?llvError, <<"Failed to open log file ~ts with error ~s">>, [FileName, file:format_error(Reason)]),
         {ok, TemState#state{flap = true}}
   end.

handleCall(mGetLogLevel, #state{level = Level}) ->
   {reply, Level};
handleCall({mSetLogLevel, Level}, #state{fBName = FBName} = State) ->
   case lgUtil:validateLogLevel(Level) of
      false ->
         {reply, {error, bad_loglevel}};
      LevelMask ->
         ?INT_LOG(?llvNotice, <<"Changed loglevel of ~s to ~p">>, [FBName, Level]),
         {reply, ok, State#state{level = LevelMask}}
   end;
handleCall({mSetLogHwm, Hwm}, #state{shaper = Shaper, fBName = FBName} = State) ->
   case checkOpts([{hwm, Hwm}], true) of
      false ->
         {reply, {error, badHwm}};
      _ ->
         NewShaper = Shaper#lgShaper{hwm = Hwm},
         ?INT_LOG(?llvNotice, <<"Changed loghwm of ~ts to ~p">>, [FBName, Hwm]),
         {reply, {lastHwm, Shaper#lgShaper.hwm}, State#state{shaper = NewShaper}}
   end;
handleCall(mRotate, State = #state{fBName = FBName}) ->
   {_, NewState} = handleInfo({mRotate, FBName}, State),
   {reply, ok, NewState};
handleCall(_Msg, _State) ->
   ?ERR(<<"~p call receive unexpect msg ~p ~n ">>, [?MODULE, _Msg]),
   {reply, ok}.

handleEvent({mWriteLog, Message}, #state{fBName = FBName, level = Level, shaper = Shaper, fmtTer = FmtTer, fmtCfg = FmtCfg} = State) ->
   case lgUtil:isLoggAble(Message, Level, {lgBkdFile, FBName}) of
      true ->
         #lgMsg{timestamp = Timestamp, severity = Severity} = Message,
         case lgUtil:checkHwm(Shaper) of
            {true, _Drop, NewShaper} ->
               {noreply, writeLog(State#state{shaper = NewShaper}, Timestamp, Severity, FmtTer:format(Message, FmtCfg))};
            {drop, Drop, NewShaper} ->
               TemState =
                  case Drop =< 0 of
                     true ->
                        State;
                     _ ->
                        ReportStr = eFmt:format(<<"lgBkdFile dropped ~p messages in the last second that exceeded the limit of ~p messages/sec">>, [Drop, NewShaper#lgShaper.hwm]),
                        NowMs = lgTime:nowMs(),
                        NowStr = lgUtil:msToBinStr(NowMs),
                        ReportMsg = #lgMsg{severity = ?llvWarning, pid = self(), node = node(), module = ?MODULE, function = ?FUNCTION_NAME, line = ?LINE, metadata = [], datetime = NowStr, timestamp = NowMs, message = ReportStr, destinations = []},
                        writeLog(State, NowMs, ?llvWarning, FmtTer:format(ReportMsg, FmtCfg))
                  end,
               {noreply, writeLog(TemState#state{shaper = NewShaper}, Timestamp, Severity, FmtTer:format(Message, FmtCfg))};
            {false, _, NewShaper} ->
               {noreply, State#state{shaper = NewShaper}}
         end;
      _ ->
         kpS
   end;
handleEvent(_Msg, _State) ->
   ?ERR(<<"~p event receive unexpect msg ~p ~n ">>, [?MODULE, _Msg]),
   kpS.

handleInfo({mRotate, FBName}, #state{fBName = FBName, date = Date} = State) ->
   NewState = closeFile(State),
   scheduleRotation(Date, FBName),
   {noreply, NewState};
handleInfo({mRotate, _FBName}, State) ->
   {noreply, State};
handleInfo({mShaperExpired, FBName}, #state{shaper = Shaper, fBName = FBName, fmtTer = FmtTer, fmtCfg = FmtCfg} = State) ->
   case Shaper#lgShaper.dropped of
      0 ->
         ignore;
      Dropped ->
         ReportStr = eFmt:format(<<"lgBkdFile dropped ~p messages in the last second that exceeded the limit of ~p messages/sec">>, [Dropped, Shaper#lgShaper.hwm]),
         NowMs = lgTime:nowMs(),
         NowStr = lgUtil:msToBinStr(NowMs),
         ReportMsg = #lgMsg{severity = ?llvWarning, pid = self(), node = node(), module = ?MODULE, function = ?FUNCTION_NAME, line = ?LINE, metadata = [], datetime = NowStr, timestamp = NowMs, message = ReportStr, destinations = []},
         writeLog(State, NowMs, ?llvWarning, FmtTer:format(ReportMsg, FmtCfg))
   end,
   {noreply, State#state{shaper = Shaper#lgShaper{dropped = 0}}};
handleInfo(_Msg, _State) ->
   ?ERR(<<"~p info receive unexpect msg ~p ~n ">>, [?MODULE, _Msg]),
   kpS.

terminate(_Reason, State) ->
   %% leaving this function call unmatched makes dialyzer cranky
   _ = closeFile(State),
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

writeLog(#state{fileName = FileName, fd = Fd, inode = Inode, cTime = CTime, flap = Flap, size = RotSize, rotator = Rotator, lastCheck = LastCheck, checkInt = CheckInt, syncSize = SyncSize, syncInt = SyncInt} = State, Timestamp, Level, Msg) ->
   case isWriteCheck(Fd, LastCheck, CheckInt, FileName, Inode, CTime, Timestamp) of
      true ->
         %% need to check for rotation
         case Rotator:ensureLogFile(FileName, Fd, Inode, CTime, {SyncSize, SyncInt}) of
            {ok, NewFD, NewInode, NewCTime, FileSize} ->
               case RotSize > 0 andalso FileSize > RotSize of
                  true ->
                     NewState = closeFile(State),
                     %% go around the loop again, we'll do another rotation check and hit the next clause of ensureLogFile
                     writeLog(NewState, Timestamp, Level, Msg);
                  _ ->
                     %% update our last check and try again
                     NewState = State#state{lastCheck = Timestamp, fd = NewFD, inode = NewInode, cTime = NewCTime},
                     writeFile(NewState, Level, Msg)
               end;
            {error, Reason} ->
               ?IIF(Flap, State, begin ?INT_LOG(?llvError, <<"Failed to reopen log file ~ts with error ~s">>, [FileName, file:format_error(Reason)]), State#state{flap = true} end)
         end;
      _ ->
         writeFile(State, Level, Msg)
   end.

writeFile(#state{fd = Fd, fileName = FileName, flap = Flap, syncOn = SyncOn} = State, Level, Msg) ->
   %% delayed_write doesn't report errors
   _ = file:write(Fd, unicode:characters_to_binary(Msg)),
   case (Level band SyncOn) =/= 0 of
      true ->
         %% force a sync on any message that matches the 'syncOn' bitmask
         NewFlap =
            case file:datasync(Fd) of
               {error, Reason} when Flap == false ->
                  ?INT_LOG(?llvError, <<"Failed to write log message to file ~ts: ~s">>, [FileName, file:format_error(Reason)]),
                  true;
               ok ->
                  false;
               _ ->
                  Flap
            end,
         State#state{flap = NewFlap};
      _ ->
         State
   end.

isWriteCheck(Fd, LastCheck, CheckInt, Name, Inode, CTime, Timestamp) ->
   DiffTime = abs(Timestamp - LastCheck),
   case DiffTime >= CheckInt orelse Fd == undefined of
      true ->
         true;
      _ ->
         % We need to know if the file has changed "out from under eLog" so we don't write to an invalid Fd
         {Result, _FInfo} = lgUtil:isFileChanged(Name, Inode, CTime),
         Result
   end.

%% Convert the config into a gen_event handler ID
configToId(Config) ->
   case lgUtil:get_opt(id, Config, undefined) of
      undefined ->
         case lgUtil:get_opt(file, Config, undefined) of
            undefined ->
               erlang:error(no_file);
            File ->
               {?MODULE, File}
         end;
      Id ->
         {?MODULE, Id}
   end.

checkOpts([], IsFile) ->
   ?IIF(IsFile, true, {error, no_file_name});
checkOpts([{file, _File} | Tail], _IsFile) ->
   checkOpts(Tail, true);
checkOpts([{level, Level} | Tail], IsFile) ->
   ?IIF(lgUtil:validateLogLevel(Level) =/= false, checkOpts(Tail, IsFile), {error, {invalid_log_level, Level}});
checkOpts([{size, Size} | Tail], IsFile) when is_integer(Size), Size >= 0 ->
   checkOpts(Tail, IsFile);
checkOpts([{count, Count} | Tail], IsFile) when is_integer(Count), Count >= 0 ->
   checkOpts(Tail, IsFile);
checkOpts([{rotator, Rotator} | Tail], IsFile) when is_atom(Rotator) ->
   checkOpts(Tail, IsFile);
checkOpts([{hwm, HighWaterMark} | Tail], IsFile) when is_integer(HighWaterMark), HighWaterMark >= 0 ->
   checkOpts(Tail, IsFile);
checkOpts([{date, _Date} | Tail], IsFile) ->
   checkOpts(Tail, IsFile);
checkOpts([{syncInt, SyncInt} | Tail], IsFile) when is_integer(SyncInt), SyncInt >= 0 ->
   checkOpts(Tail, IsFile);
checkOpts([{syncSize, SyncSize} | Tail], IsFile) when is_integer(SyncSize), SyncSize >= 0 ->
   checkOpts(Tail, IsFile);
checkOpts([{checkInt, CheckInt} | Tail], IsFile) when is_integer(CheckInt), CheckInt >= 0; CheckInt == always ->
   checkOpts(Tail, IsFile);
checkOpts([{syncOn, Level} | Tail], IsFile) ->
   ?IIF(lgUtil:validateLogLevel(Level) =/= false, checkOpts(Tail, IsFile), {error, {invalid_sync_on, Level}});
checkOpts([{fmtTer, Fmt} | Tail], IsFile) when is_atom(Fmt) ->
   checkOpts(Tail, IsFile);
checkOpts([{fmtCfg, FmtCfg} | Tail], IsFile) when is_list(FmtCfg) ->
   checkOpts(Tail, IsFile);
checkOpts([{flushQueue, FlushCfg} | Tail], IsFile) when is_boolean(FlushCfg) ->
   checkOpts(Tail, IsFile);
checkOpts([{flushThr, Thr} | Tail], IsFile) when is_integer(Thr), Thr >= 0 ->
   checkOpts(Tail, IsFile);
checkOpts([{id, _} | Tail], IsFile) ->
   checkOpts(Tail, IsFile);
checkOpts([Other | _Tail], _IsFile) ->
   {error, {invalid_opt, Other}}.

scheduleRotation(undefined, _FBName) ->
   ok;
scheduleRotation(Date, FBName) ->
   erlang:send_after(lgUtil:calcNextRotateMs(Date), self(), {mRotate, FBName}),
   ok.

closeFile(#state{fBName = FBName, fd = Fd} = State) ->
   case Fd of
      undefined -> State;
      _ ->
         %% Flush and close any file handles.
         %% delayed write can cause file:close not to do a close
         _ = file:datasync(Fd),
         _ = file:close(Fd),
         _ = file:close(Fd),
         NewFileName = lgUtil:parsePath(FBName),
         State#state{fileName = NewFileName, fd = undefined}
   end.
