-module(todo_file_appender_http_handler).

-behavior(cowboy_rest).

-export([
    init/2,
    terminate/3
]).

-export([
    allowed_methods/2,
    known_methods/2,
    content_types_accepted/2
]).

-export([
    from_json/2
]).

-define(KNOWN_METHODS, [<<"OPTIONS">>, <<"POST">>]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.

allowed_methods(Req, State) ->
    {?KNOWN_METHODS, Req, State}.

known_methods(Req, State) ->
    {?KNOWN_METHODS, Req, State}.

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, from_json}
    ], Req, State}.

from_json(Req, State) ->
    post(Req, State).

post(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_urlencoded_body(Req),

    case todo_file_appender_http_helpers:get_body(Body, Req1) of
        {ok, Input, Req2} ->
            ok = todo_file_appender_model:append(Input),
            {true, todo_file_appender_http_helpers:reply(200, Input, Req2), State};
        {error, empty, Req2} -> 
            {false, Req2, State}
    end.
