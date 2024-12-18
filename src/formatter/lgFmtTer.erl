-module(lgFmtTer).

-include("lgDef.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   format/2
   , format/3
   , fmtCfg/1
   , errSeverity/1
]).

%%
%% API Functions
%%

%% @doc Provides a generic, default formatting for log messages using a semi-iolist as configuration.  Any iolist allowed
%% elements in the configuration are printed verbatim.  Atoms in the configuration are treated as metadata properties
%% and extracted from the log message.  Optionally, a tuple of {atom(),semi-iolist()} can be used.  The atom will look
%% up the property, but if not found it will use the semi-iolist() instead.  These fallbacks can be similarly nested
%% or refer to other properties, if desired. You can also use a {atom, semi-iolist(), semi-iolist()} fmtTer, which
%% acts like a ternary operator's true/false branches.
%%
%% The metadata properties datetime, message, severity, pid, line, module, and function  will always exist.

%% @doc使用半ioiolist作为配置为日志消息提供通用的默认格式。允许的任何iolist配置中的
%%元素按原样打印。配置中的原子被视为元数据属性
%%并从日志消息中提取。可选地，可以使用{atom（），semi-iolist（）}的元组。原子会看起来
%%的属性，但如果找不到，它将使用semi-iolist（）代替。这些后备广告可以类似地嵌套
%%或参考其他属性（如果需要）。您还可以使用{atom，semi-iolist（），semi-iolist（）} fmtTer，
%%的作用类似于三元运算符的true / false分支。
%%
%%元数据属性日期，时间，消息，严重性和严重性将始终存在。
%%如果使用解析器转换，则pid，文件，行，模块和函数的属性将始终存在

%%
%% Example:
%%
%%    `["Foo"]' -> "Foo", regardless of message content.
%%
%%    `[message]' -> The content of the logged message, alone.
%%
%%    `[{pid,"Unknown Pid"}]' -> "?.?.?" if pid is in the metadata, "Unknown Pid" if not.
%%
%%    `[{pid, ["My pid is ", pid], ["Unknown Pid"]}]' -> if pid is in the metada print "My pid is ?.?.?", otherwise print "Unknown Pid"
%% @end

-spec format(lgMsg(), list()) -> any().
format(LgMsg, Config) ->
   [output(V, LgMsg) || V <- Config].

format(LgMsg, Config, Colors) ->
   [
      case V of
         color -> outColor(Colors, LgMsg);
         _ -> output(V, LgMsg)
      end || V <- Config
   ].

fmtCfg([]) ->
   [datetime, sev, node, <<"|">>, pid, <<"|">>, module, <<"|">>, function, <<"|">>, line, <<"|">>, message, <<"\n">>];
fmtCfg(MetaWhitelist) ->
   [datetime, sev, node, <<"|">>, pid, <<"|">>, module, <<"|">>, function, <<"|">>, line, <<"|">>] ++
      [{M, [atom_to_binary(M), <<"=">>, M, "|"], [<<>>]} || M <- MetaWhitelist] ++ [message, <<"\n">>].

%% @doc Print the format string `Fmt' with `Args' safely with a size
%% limit of `Limit'. If the format string is invalid, or not enough
%% arguments are supplied 'FORMAT ERROR' is printed with the offending
%% arguments. The caller is NOT crashed.

unsafeFormat(Fmt, Args) ->
   try eFmt:formatIol(Fmt, Args)
   catch
      _:_ -> eFmt:formatIol(<<"FORMAT ERROR SAFE: ~p ~p">>, [Fmt, Args])
   end.

safeFormat(Fmt, Args, Limit) ->
   try eFmt:formatIol(Fmt, Args, Limit)
   catch
      _:_ ->
         eFmt:formatIol(<<"FORMAT ERROR UNSAFE: ~p ~p">>, [Fmt, Args], Limit)
   end.

% Level, Pid, Node, Module, Function, FunctionArity, Line

-define(FixMd, [pid, node, module, function, line]).

-spec output(term(), lgMsg()) -> iolist().
output(message, LgMsg) ->
   #lgMsg{msgFormat = Format, msgArgs = Args, msgSafety = Safety, msgFormatSize = Size} = LgMsg,
   ?lgCASE(Args =/= [] andalso Args =/= undefined, ?lgCASE(Safety == safe, safeFormat(Format, Args, [{charsLimit, Size}]), unsafeFormat(Format, Args)), Format);
output(datetime, LgMsg) -> lgUtil:msToBinStr(LgMsg#lgMsg.timestamp);
output(pid, LgMsg) -> pid_to_list(LgMsg#lgMsg.pid);
output(node, _LgMsg) -> ?eLogCfg:get(?eLogNodeName);
output(module, LgMsg) -> atom_to_binary(LgMsg#lgMsg.module, utf8);
output(function, LgMsg) -> atom_to_binary(LgMsg#lgMsg.function, utf8);
output(line, LgMsg) -> integer_to_binary(LgMsg#lgMsg.line);
output(severity, LgMsg) -> loSeverity(LgMsg#lgMsg.severity);
output(upSeverity, LgMsg) -> upSeverity(LgMsg#lgMsg.severity);
output(blank, _LgMsg) -> <<" ">>;
output(node, _LgMsg) -> ?eLogCfg:get(?eLogNodeName);
output(sev, LgMsg) -> sevSeverity(LgMsg#lgMsg.severity);
output(metadata, LgMsg) -> mdJoin(LgMsg#lgMsg.metadata, <<"|">>, <<>>);
output({blank, Fill}, _LgMsg) -> Fill;
output({metadata, IntSep, FieldSep}, LgMsg) ->
   MD = lists:keysort(1, LgMsg#lgMsg.metadata), mdJoin(MD, IntSep, FieldSep, <<>>);
output({pterm, Key}, _LgMsg) ->
   makeStr(getPTerm(Key, <<"">>));
output({pterm, Key, Default}, _LgMsg) ->
   makeStr(getPTerm(Key, Default));
output(Prop, LgMsg) when is_atom(Prop) ->
   makeStr(getMdKey(Prop, LgMsg, <<"Undef">>));
output({Prop, Default}, LgMsg) when is_atom(Prop) ->
   makeStr(getMdKey(Prop, LgMsg, output(Default, LgMsg)));
output({Prop, Present, Absent}, LgMsg) when is_atom(Prop) ->
   case getMdKey(Prop, LgMsg) of
      undefined ->
         [output(V, LgMsg) || V <- Absent];
      _ ->
         [output(V, LgMsg) || V <- Present]
   end;
output({Prop, Present, Absent, Width}, LgMsg) when is_atom(Prop) ->
   %% sort of like a poor man's ternary operator
   case getMdKey(Prop, LgMsg) of
      undefined ->
         [output(V, LgMsg, Width) || V <- Absent];
      _ ->
         [output(V, LgMsg, Width) || V <- Present]
   end;
output(Other, _) -> makeStr(Other).

output(message, LgMsg, _Width) ->
   #lgMsg{msgFormat = Format, msgArgs = Args, msgSafety = Safety, msgFormatSize = Size} = LgMsg,
   ?lgCASE(Args =/= [] andalso Args =/= undefined, ?lgCASE(Safety == safe, safeFormat(Format, Args, [{charsLimit, Size}]), unsafeFormat(Format, Args)), Format);
output(datetime, LgMsg, _Width) -> lgUtil:msToBinStr(LgMsg#lgMsg.timestamp);
output(pid, LgMsg, _Width) -> pid_to_list(LgMsg#lgMsg.pid);
output(node, _LgMsg, _Width) -> ?eLogCfg:get(?eLogNodeName);
output(module, LgMsg, _Width) -> atom_to_binary(LgMsg#lgMsg.module, utf8);
output(function, LgMsg, _Width) -> atom_to_binary(LgMsg#lgMsg.function, utf8);
output(line, LgMsg, _Width) -> integer_to_binary(LgMsg#lgMsg.line);
output(severity, LgMsg, _Width) -> loSeverity(LgMsg#lgMsg.severity);
output(upSeverity, LgMsg, _Width) -> upSeverity(LgMsg#lgMsg.severity);
output(blank, _LgMsg, _Width) -> <<" ">>;
output(node, _LgMsg, _Width) -> ?eLogCfg:get(?eLogNodeName);
output(sev, LgMsg, _Width) -> sevSeverity(LgMsg#lgMsg.severity);
output({blank, Fill}, _LgMsg, _Width) -> Fill;
output(metadata, LgMsg, _Width) -> mdJoin(LgMsg#lgMsg.metadata, <<"|">>, <<>>);
output({metadata, IntSep, FieldSep}, LgMsg, _Width) ->
   MD = lists:keysort(1, LgMsg#lgMsg.metadata), mdJoin(MD, IntSep, FieldSep, <<>>);
output({pterm, Key}, _LgMsg, _Width) -> makeStr(getPTerm(Key, <<"">>));
output({pterm, Key, Default}, _LgMsg, _Width) -> makeStr(getPTerm(Key, Default));
output(Prop, LgMsg, Width) when is_atom(Prop) ->
   makeStr(getMdKey(Prop, LgMsg, <<"Undef">>), Width);
output({Prop, Default}, LgMsg, Width) when is_atom(Prop) ->
   makeStr(getMdKey(Prop, LgMsg, output(Default, LgMsg)), Width);
output(Other, _, Width) -> makeStr(Other, Width).

outColor([], _LgMsg) -> <<>>;
outColor(Colors, LgMsg) ->
   case lists:keyfind(LgMsg#lgMsg.severity, 1, Colors) of
      {_, Color} -> Color;
      _ -> <<>>
   end.

-spec makeStr(any()) -> iolist().
makeStr(A) when is_atom(A) -> atom_to_binary(A);
makeStr(P) when is_pid(P) -> list_to_binary(pid_to_list(P));
makeStr(B) when is_binary(B) -> B;
makeStr(Other) -> eFmt:format(<<"~p">>, [Other]).

makeList(A) when is_atom(A) -> atom_to_list(A);
makeList(P) when is_pid(P) -> pid_to_list(P);
makeList(Other) -> binary_to_list(eFmt:format(<<"~p">>, [Other])).

makeStr(A, W) when is_integer(W) -> makeStr(left, makeList(A), W);
makeStr(A, {Align, W}) when is_integer(W) ->
   makeStr(Align, makeList(A), W);
makeStr(A, _W) -> makeStr(A).

makeStr(left, Str, W) ->
   list_to_binary(string:left(Str, W));
makeStr(centre, Str, W) ->
   list_to_binary(string:centre(Str, W));
makeStr(right, Str, W) ->
   list_to_binary(string:right(Str, W));
makeStr(_, Str, W) ->
   list_to_binary(string:left(Str, W)).

%% persistent term was introduced in OTP 21.2, so
%% if we're running on an older OTP, just return the
%% default value.
getPTerm(Key, Default) ->
   persistent_term:get(Key, Default).

getMdKey(Key, LgMsg) ->
   getMdKey(Key, LgMsg, undefined).

getMdKey(Key, LgMsg, Default) ->
   case lists:keyfind(Key, 1, LgMsg#lgMsg.metadata) of
      false ->
         case lists:member(Key, ?FixMd) of
            true ->
               output(Key, LgMsg);
            _ ->
               Default
         end;
      {_Key, Fun} when is_function(Fun) ->
         runFun(Fun, Default);
      {_Key, Value} ->
         Value
   end.

runFun(Fun, Default) ->
   try Fun()
   catch
      _:_ ->
         Default
   end.

loSeverity(?llvDebug) -> <<"debug">>;
loSeverity(?llvInfo) -> <<"info">>;
loSeverity(?llvNotice) -> <<"notice">>;
loSeverity(?llvWarning) -> <<"warning">>;
loSeverity(?llvError) -> <<"error">>;
loSeverity(?llvCritical) -> <<"critical">>;
loSeverity(?llvAlert) -> <<"alert">>;
loSeverity(?llvEmergency) -> <<"emergency">>;
loSeverity(?llvNone) -> <<"none">>.

errSeverity(?llvDebug) -> 'debug';
errSeverity(?llvInfo) -> 'info';
errSeverity(?llvNotice) -> 'notice';
errSeverity(?llvWarning) -> 'warning';
errSeverity(?llvError) -> 'error';
errSeverity(?llvCritical) -> 'critical';
errSeverity(?llvAlert) -> 'alert';
errSeverity(?llvEmergency) -> 'emergency';
errSeverity(?llvNone) -> 'Undef'.

upSeverity(?llvDebug) -> <<"DEBUG">>;
upSeverity(?llvInfo) -> <<"INFO">>;
upSeverity(?llvNotice) -> <<"NOTICE">>;
upSeverity(?llvWarning) -> <<"WARNING">>;
upSeverity(?llvError) -> <<"ERROR">>;
upSeverity(?llvCritical) -> <<"CRITICAL">>;
upSeverity(?llvAlert) -> <<"ALERT">>;
upSeverity(?llvEmergency) -> <<"EMERGENCY">>;
upSeverity(?llvNone) -> <<"NONE">>.

sevSeverity(?llvDebug) -> <<"[D]">>;
sevSeverity(?llvInfo) -> <<"[I]">>;
sevSeverity(?llvNotice) -> <<"[N]">>;
sevSeverity(?llvWarning) -> <<"[W]">>;
sevSeverity(?llvError) -> <<"[E]">>;
sevSeverity(?llvCritical) -> <<"[C]">>;
sevSeverity(?llvAlert) -> <<"[A]">>;
sevSeverity(?llvEmergency) -> <<"[E]">>;
sevSeverity(?llvNone) -> <<"[o]">>.

mdJoin([], _FieldSep, BinAcc) ->
   BinAcc;
mdJoin([{_K, V}], _FieldSep, BinAcc) ->
   <<BinAcc/binary, (eFmt:format(<<"~p">>, [V]))/binary>>;
mdJoin([{_K, V} | Left], FieldSep, BinAcc) ->
   mdJoin(Left, FieldSep, <<BinAcc/binary, (eFmt:format(<<"~p">>, [V]))/binary, FieldSep/binary>>).

mdJoin([], _IntSep, _FieldSep, BinAcc) ->
   BinAcc;
mdJoin([{K, V}], IntSep, _FieldSep, BinAcc) ->
   <<BinAcc/binary, (eFmt:format(<<"~p~s~p">>, [K, IntSep, V]))/binary>>;
mdJoin([{K, V} | Left], IntSep, FieldSep, BinAcc) ->
   mdJoin(Left, FieldSep, <<BinAcc/binary, (eFmt:format(<<"~p~s~p">>, [K, IntSep, V]))/binary, FieldSep/binary>>).





