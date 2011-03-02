%% -------------------------------------------------------------------
%%
%% Copyright (c) 2011 Heroku, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

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
