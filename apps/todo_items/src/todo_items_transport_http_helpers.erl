-module(todo_items_transport_http_helpers).

-export([
    get_body/2,
    reply/3
]).

reply(Code, Body, Req) ->
    cowboy_req:reply(Code, #{<<"content-type">> => <<"application/json">>}, jiffy:encode(Body), Req).

get_body(Body, Req) ->
    case Body of 
        [{Input, true}] ->
            try jiffy:decode(Input, [return_maps]) of
                Parsed ->
                    {ok, Parsed, Req}
            catch
                _:_ ->
                    {error, empty, reply(400, <<"Invalid json">>, Req)}
            end;
        [] ->
            {error, empty, reply(400, <<"Missing body">>, Req)};
        _ ->
            {error, empty, reply(400, <<"Bad request">>, Req)}
    end.
