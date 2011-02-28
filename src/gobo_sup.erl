-module(gobo_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         new_conn/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

new_conn(Host, Port) ->
    supervisor:start_child(?SERVER, [Host, Port]).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Child = {gobo_conn, {gobo_conn, start_link, []},
             temporary, brutal_kill, worker, [gobo_conn]},
    Strategy = {simple_one_for_one, 0, 1},
    {ok, {Strategy, [Child]}}.
