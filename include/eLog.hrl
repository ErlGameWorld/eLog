-include("lgCom.hrl").

%% 该宏用于确保eLogCfg模块初始化了 任何使用eLog的App启动后请调用一次
-define(eLogInit(), ?eLogInit(?LgDefSink)).
-define(eLogInit(Sink),
   case ets:info(?eLogEts) of
      undefined ->
         ets:new(?eLogEts, [named_table, public]),
         ets:insert(?eLogEts, {Sink, ?none}),
         lgKvsToBeam:load(?eLogCfg, [{Sink, ?none}]);
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

-define(lgNone(Format), ?lgLog(?none, Format, undefined, safe)).
-define(lgNone(Format, Args), ?lgLog(?none, Format, Args, safe)).
-define(lgNone(Metadata, Format, Args), ?lgLog(?none, Metadata, Format, Args, safe)).

-define(lgDebug(Format), ?lgLog(?debug, Format, undefined, safe)).
-define(lgDebug(Format, Args), ?lgLog(?debug, Format, Args, safe)).
-define(lgDebug(Metadata, Format, Args), ?lgLog(?debug, Metadata, Format, Args, safe)).

-define(lgInfo(Format), ?lgLog(?info, Format, undefined, safe)).
-define(lgInfo(Format, Args), ?lgLog(?info, Format, Args, safe)).
-define(lgInfo(Metadata, Format, Args), ?lgLog(?info, Metadata, Format, Args, safe)).

-define(lgNotice(Format), ?lgLog(?notice, Format, undefined, safe)).
-define(lgNotice(Format, Args), ?lgLog(?notice, Format, Args, safe)).
-define(lgNotice(Metadata, Format, Args), ?lgLog(?notice, Metadata, Format, Args, safe)).

-define(lgWarning(Format), ?lgLog(?warning, Format, undefined, safe)).
-define(lgWarning(Format, Args), ?lgLog(?warning, Format, Args, safe)).
-define(lgWarning(Metadata, Format, Args), ?lgLog(?warning, Metadata, Format, Args, safe)).

-define(lgError(Format), ?lgLog(?error, Format, undefined, safe)).
-define(lgError(Format, Args), ?lgLog(?error, Format, Args, safe)).
-define(lgError(Metadata, Format, Args), ?lgLog(?error, Metadata, Format, Args, safe)).

-define(lgCritical(Format), ?lgLog(?critical, Format, undefined, safe)).
-define(lgCritical(Format, Args), ?lgLog(?critical, Format, Args, safe)).
-define(lgCritical(Metadata, Format, Args), ?lgLog(?critical, Metadata, Format, Args, safe)).

-define(lgAlert(Format), ?lgLog(?alert, Format, undefined, safe)).
-define(lgAlert(Format, Args), ?lgLog(?alert, Format, Args, safe)).
-define(lgAlert(Metadata, Format, Args), ?lgLog(?alert, Metadata, Format, Args, safe)).

-define(lgEmergency(Format), ?lgLog(?emergency, Format, undefined, safe)).
-define(lgEmergency(Format, Args), ?lgLog(?emergency, Format, Args, safe)).
-define(lgEmergency(Metadata, Format, Args), ?lgLog(?emergency, Metadata, Format, Args, safe)).

