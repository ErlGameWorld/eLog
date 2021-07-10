-module(lgConfig).

-include("lgDef.hrl").
-include("lgCom.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   ptGet/2
   , ptSet/2
]).

ptGet(Key, Default) ->
   persistent_term:get({?eLogPtTl, Key}, Default).

ptSet(Key, Value) ->
   persistent_term:put({?eLogPtTl, Key}, Value).


