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

-module(gobo_conn).

-include("msg_pb.hrl").
-include("gobo_pb.hrl").

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% Doozer ops
-export([close/1,
         get/3,
         get/4,
         set/5,
         del/4,
         snap/2,
         delsnap/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {sock,
                pending=dict:new(),
                tag=1}).

close(Pid) ->
    gen_server:call(Pid, close).

get(Pid, Path, WantsReturn) ->
    get(Pid, Path, undefined, WantsReturn).

get(Pid, Path, Id, WantsReturn) ->
    gen_server:call(Pid, {get, Path, Id, self(), WantsReturn}).

set(Pid, Path, Cas, Value, WantsReturn) ->
    gen_server:call(Pid, {set, Path, Cas, Value, self(), WantsReturn}).

del(Pid, Path, Cas, WantsReturn) ->
    gen_server:call(Pid, {del, Path, Cas, self(), WantsReturn}).

snap(Pid, WantsReturn) ->
    gen_server:call(Pid, {snap, self(), WantsReturn}).

delsnap(Pid, Id, WantsReturn) ->
    gen_server:call(Pid, {delsnap, Id, self(), WantsReturn}).

start_link(Host, Port) ->
    gen_server:start_link(?MODULE, [Host, Port], []).

init([Host, Port]) ->
    case gen_tcp:connect(Host, Port, [binary, {packet, 4}, {active, once}]) of
        {ok, Sock} ->
            {ok, #state{sock=Sock}};
        Error ->
            {stop, Error}
    end.

handle_call(close, _From, State) ->
    {stop, normal, ok, State};
handle_call({delsnap, Id, Caller, WantsReturn}, _From, #state{tag=Tag}=State) ->
    Req = msg_pb:encode_request(#request{tag=Tag, verb=?DELSNAP, id=Id}),
    send_request(Req, State, Caller, WantsReturn);
handle_call({snap, Caller, WantsReturn}, _From, #state{tag=Tag}=State) ->
    Req = msg_pb:encode_request(#request{tag=Tag, verb=?SNAP}),
    send_request(Req, State, Caller, WantsReturn);
handle_call({get, Path, Id, Caller, WantsReturn}, _From, #state{tag=Tag}=State) ->
    Req = msg_pb:encode_request(#request{tag=Tag, verb=?GET, path=Path, id=Id}),
    send_request(Req, State, Caller, WantsReturn);
handle_call({set, Path, Cas, Value, Caller, WantsReturn}, _From, #state{tag=Tag}=State) ->
    Req = msg_pb:encode_request(#request{tag=Tag, verb=?SET, path=Path, cas=Cas, value=Value}),
    send_request(Req, State, Caller, WantsReturn);
handle_call({del, Path, Cas, Caller, WantsReturn}, _From, #state{tag=Tag}=State) ->
    Req = msg_pb:encode_request(#request{tag=Tag, verb=?DEL, path=Path, cas=Cas}),
    send_request(Req, State, Caller, WantsReturn);
handle_call(Request, _From, State) ->
    {reply, {ignore, Request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};
handle_info({tcp_closed, _}, State) ->
    {stop, normal, State};
handle_info({tcp, Sock, Data}, #state{pending=Pending}=State) ->
    Resp = msg_pb:decode_response(Data),
    #response{tag=Tag, flags=Flags} = Resp,
    Pending1 = case dict:find(Tag, Pending) of
                   {ok, {Id, Caller}} ->
                       case Flags of
                           3 ->
                               Caller ! {valid_done, Id, Resp},
                               dict:erase(Tag, Pending);
                           2 ->
                               Caller ! {error_done, Id, Resp},
                               dict:erase(Tag, Pending);
                           1 ->
                               Caller ! {valid, Id, Resp},
                               Pending
                       end
               end,
    inet:setopts(Sock, [{active, once}]),
    {noreply, State#state{pending=Pending1}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
send_request(Req, #state{sock=Sock, pending=Pending,
                         tag=Tag}=State, Caller, WantsReturn) ->
    case gen_tcp:send(Sock, Req) of
        ok ->
            case WantsReturn of
                true ->
                    Id = erlang:make_ref(),
                    {reply, {ok, Id}, State#state{pending=dict:store(Tag, {Id, Caller}, Pending),
                                                  tag=next_tag(Tag)}};
                false ->
                    {reply, ok, State#state{tag=next_tag(Tag)}}
            end;
        Error ->
            {reply, Error, State}
    end.

next_tag(Tag) ->
    (Tag + 1) rem 16384.
