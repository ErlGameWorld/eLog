-module(lgCrashLog).

%% @doc eLog crash log writer。它将error_logger错误消息以其原始格式发送到`crash_log`指定的文件, 如果`crash_log`未设置则禁用crash logging
%% Crash logs are printed safely using trunc_io via code mostly lifted from riak_err.
%% `crashLogMsgSize` 应用程序var用于指定最大值要记录的任何消息的大小。
%% `crashLogFileSize` 用于指定崩溃日志的最大大小，然后将其旋转（0将禁用）。
%% `crashLogDate` 基于时间的轮播可通过配置，语法为自述文件中记录了
%% `crashLogCount` 控制要旋转的文件数已保留。

-behaviour(gen_srv).

-include("lgDef.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   start/6
   , start_link/6

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
   fileName :: string()                          %% 文件名
   , fBName :: string()                          %% 文件的基础名字
   , fd :: pid() | undefined                     %% 文件句柄
   , inode :: integer() | undefined              %% 文件inode信息
   , cTime :: file:date_time() | undefined       %% 文件创建时间
   , maxFmtSize :: integer()                     %% 单个消息最大字节数
   , maxFileSize :: integer()                    %% 单个日志文件最大字节数
   , date :: undefined | string()                %% 旋转的时间格式
   , count :: integer()                          %% 要保留的已轮转崩溃日志的数量
   , flap = false :: boolean()                   %% 日志文件状态是否正确
   , rotator :: atom()                           %% 旋转模块实例
}).

start(FBName, MaxFmtSize, MaxFileSize, Date, Count, Rotator) ->
   gen_srv:start({local, ?MODULE}, ?MODULE, {FBName, MaxFmtSize, MaxFileSize, Date, Count, Rotator}, []).

start_link(FBName, MaxFmtSize, MaxFileSize, Date, Count, Rotator) ->
   gen_srv:start_link({local, ?MODULE}, ?MODULE, {FBName, MaxFmtSize, MaxFileSize, Date, Count, Rotator}, []).

init({FBName, MaxFmtSize, MaxFileSize, CfgDate, Count, Rotator}) ->
   process_flag(trap_exit, true),
   {ok, Date} = lgUtil:parseRotateSpec(CfgDate),
   Filename = lgUtil:parsePath(FBName),
   case Rotator:openLogFile(Filename, false) of
      {ok, Fd, Inode, CTime, _Size} ->
         scheduleRotation(Date),
         {ok, #state{fileName = Filename, fBName = FBName, fd = Fd, inode = Inode, cTime = CTime, maxFmtSize = MaxFmtSize, maxFileSize = MaxFileSize, date = Date, count = Count, rotator = Rotator}};
      {error, Reason} ->
         ?INT_LOG(?llvError, <<"Failed to open crash log file ~ts with error: ~s">>, [Filename, file:format_error(Reason)]),
         {ok, #state{fileName = Filename, fBName = FBName, maxFmtSize = MaxFmtSize, maxFileSize = MaxFileSize, date = Date, count = Count, flap = true, rotator = Rotator}}
   end.

handleCall({mWriteLog, Event}, State, _From) ->
   {Reply, NewState} = writeLog(Event, State),
   {reply, Reply, NewState};
handleCall(_Msg, _State, _From) ->
   ?ERR(<<"~p call receive unexpect msg ~p ~n ">>, [?MODULE, _Msg]),
   {reply, ok}.

handleCast(_Msg, _State) ->
   ?ERR(<<"~p cast receive unexpect msg ~p ~n ">>, [?MODULE, _Msg]),
   kpS.

handleInfo({mWriteLog, Event}, State) ->
   {_, NewState} = writeLog(Event, State),
   {noreply, NewState};
handleInfo(mRotate, #state{date = Date} = State) ->
   NewState = closeFile(State),
   scheduleRotation(Date),
   {noreply, NewState};
handleInfo(_Msg, _State) ->
   ?ERR(<<"~p info receive unexpect msg ~p ~n ">>, [?MODULE, _Msg]),
   kpS.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

scheduleRotation(undefined) -> ok;
scheduleRotation(Date) ->
   erlang:send_after(lgUtil:calcNextRotateMs(Date), self(), mRotate),
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

otherNodeSuffix(Pid) ->
   PidNode = ?IIF(is_pid(Pid), node(Pid), undefined),
   case PidNode =/=  undefined andalso PidNode =/= node() of
      true ->
         <<"** at node ", (atom_to_binary(node(Pid), utf8))/binary, " **\n">>;
      _ ->
         <<"">>
   end.

perhapsSaslReport(error_report, {Pid, Type, Report}, FmtMaxBytes) ->
   case lgUtil:isErrorReport(Type) of
      true ->
         {saslTypeToReportHead(Type), Pid, saslLimitedStr(Type, Report, FmtMaxBytes), true};
      false ->
         {ignore, ignore, ignore, false}
   end;
perhapsSaslReport(_, _, _) ->
   {ignore, ignore, ignore, false}.

saslTypeToReportHead(supervisor_report) ->
   <<"SUPERVISOR REPORT">>;
saslTypeToReportHead(crash_report) ->
   <<"CRASH REPORT">>;
saslTypeToReportHead(progress) ->
   <<"PROGRESS REPORT">>.

saslLimitedStr(supervisor_report, Report, FmtMaxBytes) ->
   Name = lgUtil:sup_get(supervisor, Report),
   Context = lgUtil:sup_get(errorContext, Report),
   Reason = lgUtil:sup_get(reason, Report),
   Offender = lgUtil:sup_get(offender, Report),
   FmtString = <<"     Supervisor: ~p~n     Context:    ~p~n     Reason:     ~s~n     Offender:   ~s~n">>,
   ReasonStr = eFmt:formatBin(<<"~p">>, [Reason], [{charsLimit, FmtMaxBytes}]),
   OffenderStr = eFmt:formatBin(<<"~p">>, [Offender], [{charsLimit, FmtMaxBytes}]),
   eFmt:format(FmtString, [Name, Context, ReasonStr, OffenderStr]);
saslLimitedStr(progress, Report, FmtMaxBytes) ->
   [
      begin
         BinStr = eFmt:formatBin(<<"~p">>, [Data], [{charsLimit, FmtMaxBytes}]),
         eFmt:formatBin(<<"    ~16w: ~s~n">>, [Tag, BinStr])
      end || {Tag, Data} <- Report
   ];
saslLimitedStr(crash_report, Report, FmtMaxBytes) ->
   eFmt:formatBin(<<"~p">>, [Report], [{charsLimit, FmtMaxBytes}]).

writeLog(Event, #state{fileName = FileName, fd = FD, inode = Inode, cTime = CTime, flap = Flap, maxFmtSize = FmtMaxBytes, maxFileSize = RotSize, rotator = Rotator} = State) ->
   {ReportStr, Pid, MsgStr, _ErrorP} =
      case Event of
         {error, _GL, {Pid1, Fmt, Args}} ->
            {<<"ERROR REPORT">>, Pid1, eFmt:formatBin(Fmt, Args, [{charsLimit, FmtMaxBytes}]), true};
         {error_report, _GL, {Pid1, std_error, Rep}} ->
            {<<"ERROR REPORT">>, Pid1, eFmt:formatBin(<<"~p">>, [Rep], [{charsLimit, FmtMaxBytes}]), true};
         {error_report, _GL, Other} ->
            perhapsSaslReport(error_report, Other, FmtMaxBytes);
         _ ->
            {ignore, ignore, ignore, false}
      end,
   case ReportStr of
      ignore ->
         {ok, State};
      _ ->
         case Rotator:ensureLogFile(FileName, FD, Inode, CTime, false) of
            {ok, NewFD, NewInode, NewCTime, FileSize} ->
               case RotSize > 0 andalso FileSize > RotSize of
                  true ->
                     NewState = closeFile(State),
                     handleCast({mWriteLog, Event}, NewState);
                  _ ->
                     TimeBinStr = lgUtil:msToBinStr(),
                     Time = [TimeBinStr, <<" =">>, ReportStr, <<"====\n\t\t">>],
                     NodeSuffix = otherNodeSuffix(Pid),
                     Msg = eFmt:formatBin(<<"~s~s~s~n">>, [Time, MsgStr, NodeSuffix]),
                     case file:write(NewFD, unicode:characters_to_binary(Msg)) of
                        {error, Reason} when Flap == false ->
                           ?INT_LOG(?llvError, <<"Failed to write log message to file ~ts: ~s">>, [FileName, file:format_error(Reason)]),
                           {ok, State#state{fd = NewFD, inode = NewInode, cTime = NewCTime, flap = true}};
                        ok ->
                           {ok, State#state{fd = NewFD, inode = NewInode, cTime = NewCTime, flap = false}};
                        _ ->
                           {ok, State#state{fd = NewFD, inode = NewInode, cTime = NewCTime}}
                     end
               end;
            {error, Reason} ->
               ?IIF(Flap, {ok, State}, begin ?INT_LOG(?llvError, <<"Failed to reopen crash log ~ts with error: ~s">>, [FileName, file:format_error(Reason)]), {ok, State#state{flap = true}} end)
         end
   end.

