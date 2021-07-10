-module(testl).

-include("eLog.hrl").
-include("lgDef.hrl").

-compile([export_all, nowarn_export_all]).

test(Msg) ->
   ?lgDebug("IMY************* ~p~n", [Msg]).

tt() ->
   ?lgError("IMYYYYYYYYYYYYYYY111  好家伙").

ee() ->
   error_logger:error_msg("fdsfsdfsfdsfs1111111111111 ").

