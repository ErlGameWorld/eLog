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

% 主管的孩子被定义为孩子规格列表 。当主管启动时，将根据此列表从左到右依次启动子进程。主管终止时，它首先以相反的启动顺序从右到左终止其子进程。
% sup_flags() =
%     #{
%        strategy => strategy(),          % optional one_for_one | one_for_all | rest_for_one | simple_one_for_one
%        intensity => non_neg_integer(),  % optional MaxR 周期时间内最大重启次数
%        period => pos_integer()          % optional MaxT 重启时间周期  MaxT里重启次数大于MaxR
%     }
% child_spec() =
%     #{
%        id => child_id(),                % mandatory Id用来内部标识子规范
%        start => mfargs(),               % mandatory {M，F，A}
%        restart => restart(),            % optional  permanent(总是重启) | transient(异常关闭会重启即关闭原因非 normal,shutdown,{shutdown,Term}) | temporary(不会重启)
%        shutdown => shutdown(),          % optional  brutal_kill | infinity | integer
%        type => worker(),                % optional  supervisor | worker
%        modules => modules()             % optional  [Module] 假如子进程是supervisor、gen_server 或 gen_fsm，那么Module 是回调模块的名称；假如子进程是gen_event，那么Modules 应该是dynamic
%     }

init(_Args) ->
   %% set up the config, is safe even during relups
   ?eLogInit(),
   %%始终将lgEvent作为默认值启动，并确保可以根据需要启动其他gen_event东西也许是一个用于处理接收器及其策略的新API？
   SupFlags =
      #{
         strategy => one_for_one,
         intensity => 10,
         period => 60
      },

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
      shutdown => 5000,
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


