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

   %% eLogEvent进程spawn_opt配置
   %% min_bin_vheap_size: 最小二进制虚拟堆大小(word单位)，增大可减少小二进制碎片和GC频率
   %% min_heap_size: 最小堆大小(word单位)，增大可减少GC频率
   %% fullsweep_after: 多少次小GC后执行一次全量GC，增大可减少全量GC频率
   DefSpawnOpts = [
      {min_bin_vheap_size, 7 * 512 * 1024 div 8},   %% 3.5MB (word单位，64位系统1word=8字节)
      {min_heap_size, 1 * 1024 * 1024 div 8},       %% 1MB (word单位)
      {fullsweep_after, 1024}
   ],

   LgEventSpec = #{
      id => ?LgDefSink,
      start => {gen_emm, start_link, [{local, ?LgDefSink}, [{spawn_opt, DefSpawnOpts}]]},
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


