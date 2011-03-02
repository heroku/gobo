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

-module(gobo_itest).

-include("gobo.hrl").

-include_lib("eunit/include/eunit.hrl").

-export([start/0]).

start() ->
    application:start(sasl),
    application:start(crypto),
    application:start(protobuffs),
    application:start(gobo),
    gobo_itest:test().

conn_close_test() ->
    {ok, C} = gobo:connect("localhost", 8046),
    ok = gobo:close(C),
    false = erlang:is_process_alive(C).

set_get_del_test() ->
    {ok, C} = gobo:connect("localhost", 8046),
    Data = #gobo_obj{path= <<"/testing">>, value= <<"Hello, doozer!">>},
    {ok, Data1} = gobo:set(C, Data),
    ?assertNot(Data =:= Data1),
    ?assertEqual(Data#gobo_obj.path, Data1#gobo_obj.path),
    ?assertEqual(Data#gobo_obj.value, Data1#gobo_obj.value),
    {ok, Data2} = gobo:get(C, <<"/testing">>),
    ?assertMatch(Data1, Data2),
    ok = gobo:del(C, Data2),
    {error, not_found} = gobo:get(C, <<"/testing">>),
    ok = gobo:close(C).

get_snap_set_get_test() ->
    {ok, C} = gobo:connect("localhost", 8046),
    Data = #gobo_obj{path= <<"/testing/2">>, value= <<"1">>},
    {ok, Data1} = gobo:set(C, Data),
    %% Make a snapshot
    {ok, SnapId} = gobo:snap(C),
    %% Delete data
    ok = gobo:del(C, Data1),
    %% %% Verify data is still in snapshot
    {ok, Data2} = gobo:get(C, <<"/testing/2">>, SnapId),
    ?assert(Data1 =:= Data2),
    %% %% Verify data isn't in the current version
    {error, not_found} = gobo:get(C, <<"/testing/2">>),
    ok = gobo:delsnap(C, SnapId),
    {error, not_found} = gobo:get(C, <<"/testing/2">>, SnapId),
    ok = gobo:close(C).
