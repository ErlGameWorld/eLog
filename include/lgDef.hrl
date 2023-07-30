-include("lgCom.hrl").
%% 应用名字
-define(LgAppName, eLog).

%% 错误日志宏定义
-define(ERR(Format), error_logger:error_msg(Format)).
-define(ERR(Format, Args), error_logger:error_msg(Format, Args)).

-define(Print(Args), io:format("IMY************~p~n", [Args])).

%% 额外进程字典key
-define(PdMdKey, pdLgMd).
-define(LgTrackSink, '_trace_sink').

%% 旋转日志超时时间
-define(LgRotateTimeout, 100000).

%% 部分默认配置值
-define(LgDefTracer, lgDefTracer).
-define(LgErrLogSink, errLoggerEvent).

%% lgBkdConsole的选项
-type lgConsoleOpt() ::
   {id, atom() | {atom(), atom()}} |               %% 接收器 id
   {level, lgAtomLevel() | atom()} |               %% 接收器 等级级别
   {use_stderr, boolean()} |                       %% 错误输出io
   {group_leader, false | pid() | atom()} |        %% group_leader
   {fmtTer, atom()} |                              %% 日志格式化的模块名
   {fmtCfg, list()}.                               %% 日志格式化的格式

%% lgBkdFile的选项
-type lgFileOpt() ::
   {id, atom()} |                                  %% 接收器 id
   {file, binary()} |                              %% base log file whole log name: logRoot ++ timeMin ++ "_" ++ this name
   {level, lgAtomLevel() | atom()} |               %% 接收器 等级级别
   {size, non_neg_integer()} |                     %% 单个日志文件最大的字节数 为0则不旋转日志文件
   {date, string()} |                              %% 日志旋转的日期配置
   {count, non_neg_integer()} |                    %% 无用
   {rotator, atom()} |                             %% 日志旋转模块
   {hwm, non_neg_integer()} |                      %% 每一秒能能接收的日志数量  undefined 则不限制每秒接受的日志数量
   {flushQueue, boolean()} |                       %% 对于特定的接收器事件队列刷新，对于接收器 请使用改选项
   {flushThr, non_neg_integer()} |                 %% 如果flush_queue为true，则可以设置消息队列长度阈值，如果超过该阈值处将开始丢弃消息。默认阈值为0，这意味着如果flush_queue为true，则超过高水位标记时将丢弃消息，而不管消息队列的长度如何。：
   {syncInt, non_neg_integer()} |                  %% 日志文件同步间隔
   {syncSize, non_neg_integer()} |                 %% 日志文件同步大小
   {syncOn, lgAtomLevel()} |                       %% 日志文件立即同步的 日志登记级别
   {checkInt, non_neg_integer()} |                 %% 日志文件检查的时间间隔
   {fmtTer, atom()} |                              %% 日志格式化的模块
   {fmtCfg, term()}.                               %% 日志格式化的格式

%% BkdFile选项默认值
-define(LgDefLogLevel, info).
-define(LgDefRotateSize, 10485760).      %% 10mb
-define(LgDefRotateDate, <<"$D0">>).         %% midnight
-define(LgDefRotateCnt, 5).
-define(LgDefRotateMod, lgRotatorIns).
-define(LgDefSyncLevel, error).
-define(LgDefSyncInt, 1000).             %% 单位毫秒
-define(LgDefSyncSize, 1024 * 64).       %% 64kb
-define(LgDefCheckInt, 1000).            %% 单位毫秒
-define(LgDefCheckHwm, undefined).
-define(LgDefFlushQueue, false).
-define(LgDefFlushThr, 10).
-define(LgDefFmtTer, lgFmtTer).
-define(LgDefFormatterCfg, []).
-define(LgDefLogRoot, <<"./log">>).



%% 默认日志文件选项
-define(LgDefHandler,
   [
	  % {lgBkdConsole, [{level, '>=info'}]},	
      {lgBkdFile, [{id, info}, {file, <<"info.log">>}, {level, '>=info'}, {size, 10485760}, {date, <<"$D0">>}]},
      {lgBkdFile, [{id, error}, {file, <<"error.log">>}, {level, '>=error'}, {size, 10485760}, {date, <<"$D0">>}]}
   ]).

-record(lgShaper, {
   id :: any()
   %% 每秒我们尝试传递多少消息
   , hwm = undefined :: 'undefined' | pos_integer()
   %% 这秒内我们收到了多少条消息
   , mps = 0 :: non_neg_integer()
   %% 当前秒
   , lastTime = lgTime:now() :: erlang:timestamp()
   %% 此秒内丢弃的消息数
   , dropped = 0 :: non_neg_integer()
   %% If true, flush notify messages from msg queue at overload
   %% 如果为true，则在过载时刷新来自消息队列的通知消息
   , flushQueue = true :: boolean()
   , flushThr = 0 :: integer()
   %% timer
   , timer = make_ref() :: reference()
   %% optional filter fun to avoid counting suppressed messages against Hwm totals
   %% 可选的过滤器函数，以避免对Hwm总数计算抑制消息
   , filter = undefined :: fun() %% fun(_) -> false end :: fun()
}).

-record(lgMsg, {
   severity :: lgNumLevel()
   , pid :: pid()
   , node :: node()
   , module :: module()
   , function :: atom()
   , line :: integer()
   , metadata :: [tuple()]
   , datetime :: binary()
   , timestamp :: non_neg_integer()
   , message :: list()
   , destinations :: list()
}).

-type lgShaper() :: #lgShaper{}.
-type lgMsg() :: #lgMsg{}.

-type lgAtomLevel() :: none | debug | info | notice | warning | error | critical | alert | emergency.
-type lgNumLevel() :: 0 | 1 | 2 | 4 | 8 | 16 | 32 | 64 | 128.
-type lgMaskLevel() :: 0..256.

%% 日志等级列表
-define(LgLevels, [debug, info, notice, warning, error, critical, alert, emergency, none]).

-define(LgShouldLog(Sink, Level), ?eLogCfg:get(Sink) band Level =/= 0).

-define(LgShouldLog(Level), ?eLogCfg:get(?LgDefSink) band Level =/= 0).

-define(LgNotify(Level, Pid, Format, Args),
   gen_emm:info_notify(?LgDefSink, {mWriteLog, #lgMsg{severity = Level, pid = Pid, node = node(), module = ?MODULE, function = ?FUNCTION_NAME, line = ?LINE, metadata = [], datetime = lgUtil:msToBinStr(), timestamp = lgTime:nowMs(), message = eFmt:format(Format, Args), destinations = []}})).

%%仅供内部使用仅内部非阻塞日志记录调用，当我们仍在启动大型啤酒时尝试进行日志记录（通常为错误）时，会有一些特殊处理。
-define(INT_LOG(Level, Format, Args),
   Self = self(),
   %%在生成中执行此操作，这样就不会导致从gen_event处理程序调用gen_event：which_handlers的死锁
   spawn(
      fun() ->
         case catch (gen_emm:which_epm(?LgDefSink)) of
            X when X == []; X == {'EXIT', noproc}; X == [lgBkdThrottle] ->
               %% there's no handlers yet or eLog isn't running, try again
               %% in half a second.
               timer:sleep(500),
               ?LgNotify(Level, Self, Format, Args);
            _ ->
               case ?LgShouldLog(Level) of
                  true ->
                     ?LgNotify(Level, Self, Format, Args);
                  _ ->
                     ok
               end
         end
      end)).



