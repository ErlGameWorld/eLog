-ifndef(__lgCom__).
-define(__lgCom__, 1).

%% 配置模块
-define(eLogCfg, eLogCfg).
-define(eLogEts, eLogEts).
-define(eLogPtTl, eLogPtTl).

-define(LgDefSink, eLogEvent).
-define(LgDefTruncation, 4096).

%% 日志等级定义
-define(llvDebug, 128).
-define(llvInfo, 64).
-define(llvNotice, 32).
-define(llvWarning, 16).
-define(llvError, 8).
-define(llvCritical, 4).
-define(llvAlert, 2).
-define(llvEmergency, 1).
-define(llvNone, 0).

-endif.