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

-module(gobo).

-include("gobo.hrl").
-include("msg_pb.hrl").

-export([connect/2,
         close/1,
         set/2,
         get/2,
         get/3,
         del/2,
         snap/1,
         delsnap/2]).

connect(Host, Port) ->
    gobo_sup:new_conn(Host, Port).

close(Conn) ->
    gobo_conn:close(Conn).

set(Conn, #gobo_obj{cas=Cas, path=Path, value=Value}=Obj) ->
    case gobo_conn:set(Conn, Path, Cas, Value, true) of
        {ok, Id} ->
            case receive_results(Id) of
                {ok, #response{cas=NewCas}} ->
                    {ok, Obj#gobo_obj{cas=NewCas}};
                RecvError ->
                    RecvError
            end;
        Error ->
            Error
    end.

get(Conn, Path) ->
    gobo:get(Conn, Path, undefined).

get(Conn, Path, Id) ->
    case gobo_conn:get(Conn, Path, Id, true) of
        {ok, ResultId} ->
            case receive_results(ResultId) of
                {ok, #response{cas=Cas, value=Value, id=SnapId}} ->
                    case (Cas == 0 andalso Value == <<>>) orelse
                         (Cas =:= undefined andalso Value =:= undefined) of
                        true ->
                            {error, not_found};
                        false ->
                            {ok, #gobo_obj{cas=Cas, path=Path, value=Value, id=SnapId}}
                    end;
                RecvError ->
                    RecvError
            end;
        Error ->
            Error
    end.

del(Conn, #gobo_obj{cas=Cas, path=Path}) ->
    case gobo_conn:del(Conn, Path, Cas, true) of
        {ok, Id} ->
            case receive_results(Id) of
                {ok, _Resp} ->
                    ok;
                RecvError ->
                    RecvError
            end;
        Error ->
            Error
    end.

snap(Conn) ->
    case gobo_conn:snap(Conn, true) of
        {ok, Id} ->
            case receive_results(Id) of
                {ok, #response{id=SnapId}} ->
                    {ok, SnapId};
                RecvError ->
                    RecvError
            end;
        Error ->
            Error
    end.

delsnap(Conn, Id) ->
    case gobo_conn:delsnap(Conn, Id, true) of
        {ok, ResultId} ->
            case receive_results(ResultId) of
                {ok, _Resp} ->
                    ok;
                RecvError ->
                    RecvError
            end;
        Error ->
            Error
    end.

receive_results(Id) ->
    receive
        {valid_done, Id, Results} ->
            {ok, Results};
        {error_done, Id, Results} ->
            {error, Results};
        {valid, Id, Results} ->
            {more, Results}
    after 5000 ->
            {error, timeout}
    end.
