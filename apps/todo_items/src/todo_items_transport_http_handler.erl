-module(todo_items_transport_http_handler).

-behavior(cowboy_rest).

-export([
    init/2,
    terminate/3
]).

-export([
    allowed_methods/2,
    known_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    malformed_request/2,
    delete_resource/2
]).

-export([
    from_json/2,
    to_json/2
]).

-define(KNOWN_METHODS, [<<"OPTIONS">>, <<"HEAD">>, <<"GET">>, <<"PATCH">>, <<"POST">>, <<"DELETE">>]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.

allowed_methods(Req, State) ->
    {?KNOWN_METHODS, Req, State}.

known_methods(Req, State) ->
    {?KNOWN_METHODS, Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, to_json}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, from_json}
    ], Req, State}.

malformed_request(Req, State) ->
    Method = cowboy_req:method(Req),
    UUID = cowboy_req:binding(uuid, Req),
    RequiresUUID = (Method =:= <<"PATCH">>) or (Method =:= <<"DELETE">>),
    HasUUID = UUID =/= undefined,

    Result = if
        RequiresUUID ->
            not HasUUID;
        true ->
            HasUUID
    end,
    
    {Result, Req, State}.

to_json(Req, State) ->
    get(Req, State).

from_json(Req, State) ->
    case cowboy_req:method(Req) of
        <<"PATCH">> -> patch(Req, State);
        <<"POST">> -> post(Req, State)
    end.

get(Req, State) ->
    Items = todo_items_model:find(),
    {true, todo_items_transport_http_helpers:reply(200, Items, Req), State}.

post(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_urlencoded_body(Req),

    case todo_items_transport_http_helpers:get_body(Body, Req1) of
        {ok, Input, Req2} ->
            case todo_items_model:insert(Input) of
                {error, _Reason} ->
                    {false, todo_items_transport_http_helpers:reply(500, <<"Invalid json">>, Req2), State};
                {ok, Item} ->
                    {true, todo_items_transport_http_helpers:reply(200, Item, Req), State}
            end;
        {error, empty, Req2} -> 
            {false, Req2, State}
    end.

patch(Req, State) ->
    UUID = cowboy_req:binding(uuid, Req),
    {ok, Body, Req1} = cowboy_req:read_urlencoded_body(Req),

    case todo_items_transport_http_helpers:get_body(Body, Req1) of
        {ok, Input, Req2} ->
            case todo_items_model:update(UUID, Input) of
                {error, _Reason} ->
                    {false, todo_items_transport_http_helpers:reply(500, <<"Invalid json">>, Req2), State};
                {ok, Item} ->
                    {true, todo_items_transport_http_helpers:reply(200, Item, Req), State}
            end;
        {error, empty, Req2} -> 
            {false, Req2, State}
    end.

delete_resource(Req, State) ->
    UUID = cowboy_req:binding(uuid, Req),

    Result = case todo_items_model:delete(UUID) of
        {ok, _} ->
            true;
        _ ->
            false
    end,
    
    {Result, Req, State}.
