-include("lgCom.hrl").

%% 该宏用于确保eLogCfg模块初始化了 任何使用eLog的App启动后请调用一次
-define(eLogInit(), ?eLogInit(?LgDefSink)).
-define(eLogInit(Sink),
   case ets:info(?eLogEts) of
      undefined ->
         ets:new(?eLogEts, [named_table, public]),
         ets:insert(?eLogEts, {Sink, ?llvNone}),
         lgKvsToBeam:load(?eLogCfg, [{Sink, ?llvNone}]);
      _ ->
         ignore
   end).

%% 默认的元数据
%% Level, Pid, Node, Module, Function,  Line, Other

-define(lgLog(Severity, Format, Args, Safety),
   ?lgLog(?LgDefSink, Severity, self(), node(), ?MODULE, ?FUNCTION_NAME, ?LINE, eLog:getMd(), Format, Args, ?LgDefTruncation, Safety)).

-define(lgLog(Severity, Metadata, Format, Args, Safety),
   ?lgLog(?LgDefSink, Severity, self(), node(), ?MODULE, ?FUNCTION_NAME, ?LINE, Metadata ++ eLog:getMd(), Format, Args, ?LgDefTruncation, Safety)).

-define(lgLog(Sink, Severity, Pid, Node, Module, Function, Line, Metadata, Format, Args, Size, Safety),
   case ?eLogCfg:get(Sink) band Severity of
      0 ->
         ok;
      _ ->
         eLog:doLogImpl(Severity, Pid, Node, Module, Function, Line, Metadata, Format, Args, Size, Sink, Safety)
   end).

-define(lgNone(Format), ?lgLog(?llvNone, Format, undefined, safe)).
-define(lgNone(Format, Args), ?lgLog(?llvNone, Format, Args, safe)).
-define(lgNone(Metadata, Format, Args), ?lgLog(?llvNone, Metadata, Format, Args, safe)).

-define(lgDebug(Format), ?lgLog(?llvDebug, Format, undefined, safe)).
-define(lgDebug(Format, Args), ?lgLog(?llvDebug, Format, Args, safe)).
-define(lgDebug(Metadata, Format, Args), ?lgLog(?llvDebug, Metadata, Format, Args, safe)).

-define(lgInfo(Format), ?lgLog(?llvInfo, Format, undefined, safe)).
-define(lgInfo(Format, Args), ?lgLog(?llvInfo, Format, Args, safe)).
-define(lgInfo(Metadata, Format, Args), ?lgLog(?llvInfo, Metadata, Format, Args, safe)).

-define(lgNotice(Format), ?lgLog(?llvNotice, Format, undefined, safe)).
-define(lgNotice(Format, Args), ?lgLog(?llvNotice, Format, Args, safe)).
-define(lgNotice(Metadata, Format, Args), ?lgLog(?llvNotice, Metadata, Format, Args, safe)).

-define(lgWarning(Format), ?lgLog(?llvWarning, Format, undefined, safe)).
-define(lgWarning(Format, Args), ?lgLog(?llvWarning, Format, Args, safe)).
-define(lgWarning(Metadata, Format, Args), ?lgLog(?llvWarning, Metadata, Format, Args, safe)).

-define(lgError(Format), ?lgLog(?llvError, Format, undefined, safe)).
-define(lgError(Format, Args), ?lgLog(?llvError, Format, Args, safe)).
-define(lgError(Metadata, Format, Args), ?lgLog(?llvError, Metadata, Format, Args, safe)).

-define(lgCritical(Format), ?lgLog(?llvCritical, Format, undefined, safe)).
-define(lgCritical(Format, Args), ?lgLog(?llvCritical, Format, Args, safe)).
-define(lgCritical(Metadata, Format, Args), ?lgLog(?llvCritical, Metadata, Format, Args, safe)).

-define(lgAlert(Format), ?lgLog(?llvAlert, Format, undefined, safe)).
-define(lgAlert(Format, Args), ?lgLog(?llvAlert, Format, Args, safe)).
-define(lgAlert(Metadata, Format, Args), ?lgLog(?llvAlert, Metadata, Format, Args, safe)).

-define(lgEmergency(Format), ?lgLog(?llvEmergency, Format, undefined, safe)).
-define(lgEmergency(Format, Args), ?lgLog(?llvEmergency, Format, Args, safe)).
-define(lgEmergency(Metadata, Format, Args), ?lgLog(?llvEmergency, Metadata, Format, Args, safe)).

