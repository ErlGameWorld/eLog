-module(lgBkdConsole).
%% Configuration is a proplist with the following keys:
%%`level'            - log level to use
%%`use_stderr'       - either `true' or `false', defaults to false. If set to true, use standard error to output console log messages
%%`fmtTer'        - the module to use when formatting log messages. Defaults to `lgFormatTer'
%%`fmtCfg' - the format configuration string. Defaults to `time [ severity ] message'

-behaviour(gen_emm).

-include("lgDef.hrl").

-compile(inline).
-compile({inline_size, 128}).

-define(TERSE_FORMAT, [datetime, color, sev, pid,module, <<":">>, function, <<"|">>, line, <<"|">>, message]).
-define(LgDefConsoleFmtCfg, ?TERSE_FORMAT ++ [eol()]).
-define(LgDefConsoleOpts, [{use_stderr, false}, {group_leader, false}, {id, ?MODULE}, {fmtTer, ?LgDefFmtTer}, {fmtCfg, ?LgDefConsoleFmtCfg}]).

-export([
   init/1
   , handleCall/2
   , handleEvent/2
   , handleInfo/2
   , terminate/2
   , code_change/3
]).

-record(state, {
   id :: atom() | {atom(), any()}
   , level :: lgMaskLevel()
   , out = user :: user | standard_error | pid()
   , fmtTer :: atom()
   , fmtCfg :: any()
   , colors = [] :: list()
}).

-spec init([lgConsoleOpt(), ...]) -> {ok, #state{}} | {error, atom()}.
init(Opts) ->
   case isNewStyleConsole() of
      false ->
         Msg = "eLog's console backend is incompatible with the 'old' shell, not enabling it",
         %% be as noisy as possible, log to every possible place
         try alarm_handler:set_alarm({?MODULE, "WARNING: " ++ Msg})
         catch
            _:_ -> error_logger:warning_msg(Msg ++ "~n")
         end,
         io:format("WARNING: " ++ Msg ++ "~n"),
         ?INT_LOG(?llvWarning, Msg, []),
         {error, {fatal, old_shell}};
      _ ->
         true = checkOpts(Opts),
         CfgColors = ?IIF(lgUtil:get_env(colored, true), lgUtil:get_env(colors, []), []),
         Colors = [{lgUtil:levelToNum(Level), ColorStr} || {Level, ColorStr} <- CfgColors],
         Level = lgUtil:get_opt(level, Opts, undefined),
         LevelMask = lgUtil:configToMask(Level),
         [UseErr, GroupLeader, Id, FmtTer, FmtCfg] = [lgUtil:get_opt(Key, Opts, Def) || {Key, Def} <- ?LgDefConsoleOpts],
         Out = ?IIF(UseErr, standard_error, ?IIF(GroupLeader == false, user, begin erlang:monitor(process, GroupLeader), GroupLeader end)),
         {ok, #state{level = LevelMask, id = Id, out = Out, fmtTer = FmtTer, fmtCfg = FmtCfg, colors = Colors}}
   end.

checkOpts([]) -> true;
checkOpts([{id, {?MODULE, _}} | T]) ->
   checkOpts(T);
checkOpts([{level, Level} | T]) ->
   ?IIF(lgUtil:validateLogLevel(Level) =/= false, checkOpts(T), {error, {bad_level, Level}});
checkOpts([{use_stderr, Flag} | T]) when is_boolean(Flag) ->
   checkOpts(T);
checkOpts([{fmtTer, M} | T]) when is_atom(M) ->
   checkOpts(T);
checkOpts([{fmtCfg, C} | T]) when is_list(C) ->
   checkOpts(T);
checkOpts([{group_leader, L} | T]) when is_pid(L) ->
   checkOpts(T);
checkOpts([H | _]) ->
   {error, {invalid_opt, H}}.

handleCall(mGetLogLevel, State) ->
   {reply, State#state.level};
handleCall({mSetLogLevel, Level}, State) ->
   case lgUtil:validateLogLevel(Level) of
      false ->
         {reply, {error, bad_loglevel}};
      LevelMask ->
         {reply, ok, State#state{level = LevelMask}}
   end;
handleCall({mRotate, _}, _State) ->
   {reply, ok};
handleCall(mRotate, _State) ->
   {reply, ok, _State};
handleCall(_Msg, _State) ->
   ?ERR(<<"~p call receive unexpect msg ~p ~n ">>, [?MODULE, _Msg]),
   {reply, ok}.

handleEvent({mWriteLog, Message}, #state{level = Level, out = Out, fmtTer = FmtTer, fmtCfg = FmtCfg, colors = Colors, id = ID}) ->
   case lgUtil:isLoggAble(Message, Level, ID) of
      true ->
         io:put_chars(Out, FmtTer:format(Message, FmtCfg, Colors)),
         kpS;
      _ ->
         kpS
   end;
handleEvent(_Msg, _State) ->
   ?ERR(<<"~p event receive unexpect msg ~p ~n ">>, [?MODULE, _Msg]),
   kpS.

handleInfo({'DOWN', _, process, Out, _}, #state{out = Out}) ->
   removeEpm;
handleInfo({mRotate, _}, _State) ->
   kpS;
handleInfo(_Msg, _State) ->
   ?ERR(<<"~p info receive unexpect msg ~p ~n">>, [?MODULE, _Msg]),
   kpS.

terminate(removeEpm, State) ->
   %% have to do this asynchronously because we're in the event handlr
   spawn(fun() -> eLog:clearTraceByDest(State#state.id) end),
   ok;
terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

eol() ->
   ?IIF(lgUtil:get_env(colored, true), <<"\e[0m\r\n">>, <<"\r\n">>).

isNewStyleConsole() ->
   %% Criteria:
   %% 1. If the user has specified '-noshell' on the command line,
   %%    then we will pretend that the new-style console is available.
   %%    If there is no shell at all, then we don't have to worry
   %%    about log events being blocked by the old-style shell.
   %% 2. Windows doesn't support the new shell, so all windows users
   %%    have is the oldshell.
   %% 3. If the user_drv process is registered, all is OK.
   %%    'user_drv' is a registered proc name used by the "new"
   %%    console driver.
   init:get_argument(noshell) =/= error orelse element(1, os:type()) =/= win32 orelse is_pid(whereis(user_drv)).
