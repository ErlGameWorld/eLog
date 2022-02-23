-module(lgHWatcherSup).

-behaviour(supervisor).

-export([
   start_link/0
]).

-export([init/1]).

start_link() ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
   SupFlags = #{strategy => simple_one_for_one, intensity => 10, period => 60},
   ChildSpecs = [
      #{
         id => lgHWatcherSrv,
         start => {lgHWatcherSrv, start_link, []},
         restart => temporary,
         shutdown => 5000,
         type => worker,
         modules => [lgHWatcherSrv]
      }
   ],
   {ok, {SupFlags, ChildSpecs}}.
