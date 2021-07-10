-ifndef(__lgCom__).
-define(__lgCom__, 1).

%% 配置模块
-define(eLogCfg, eLogCfg).
-define(eLogEts, eLogEts).
-define(eLogPtTl, eLogPtTl).

-define(LgDefSink, eLogEvent).
-define(LgDefTruncation, 4096).

%% 日志等级定义
-define(debug, 128).
-define(info, 64).
-define(notice, 32).
-define(warning, 16).
-define(error, 8).
-define(critical, 4).
-define(alert, 2).
-define(emergency, 1).
-define(none, 0).

-endif.