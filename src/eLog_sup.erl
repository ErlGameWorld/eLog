-module(eLog_sup).

-behaviour(supervisor).

-include("eLog.hrl").
-include("lgDef.hrl").

-export([
   start_link/0
]).

-export([
   init/1
]).

start_link() ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
   %% set up the config, is safe even during relups
   ?eLogInit(),
   %%始终将lgEvent作为默认值启动，并确保可以根据需要启动其他gen_event东西也许是一个用于处理接收器及其策略的新API？
   SupFlags = #{strategy => one_for_one, intensity => 10, period => 60},

   LgEventSpec = #{
      id => ?LgDefSink,
      start => {gen_emm, start_link, [{local, ?LgDefSink}]},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [dynamic]
   },

   LgHWatcherSupSpec = #{
      id => lgHWatcherSup,
      start => {lgHWatcherSup, start_link, []},
      restart => permanent,
      shutdown => infinity,
      type => supervisor,
      modules => [lgHWatcherSup]
   },

   ChildSpecs =
      case lgUtil:get_env(crashLogFile, false) of
         false ->
            [LgEventSpec, LgHWatcherSupSpec];
         File ->
            MsgMaxBytes = lgUtil:get_env(crashLogMsgSize, 65536),
            RotationSize = lgUtil:get_env(crashLogFileSize, 0),
            RotationCount = lgUtil:get_env(crashLogCount, 0),
            RotationDate = lgUtil:get_env(crashLogDate, <<"$D0">>),
            RotationMod = lgUtil:get_env(crashLogRotator, ?LgDefRotateMod),
            CrashLogSpec = #{
               id => lgCrashLog,
               start => {lgCrashLog, start_link, [File, MsgMaxBytes, RotationSize, RotationDate, RotationCount, RotationMod]},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => [lgCrashLog]
            },
            [LgEventSpec, LgHWatcherSupSpec, CrashLogSpec]
      end,

   {ok, {SupFlags, ChildSpecs}}.


