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
