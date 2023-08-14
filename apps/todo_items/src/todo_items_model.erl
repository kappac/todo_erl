-module(todo_items_model).

-include("todo_items.hrl").

-export([
    insert/1,
    update/2,
    delete/1,
    find/0
]).

-define(SCHEME_BASIC, map).
-define(SCHEME_FIELD_UUID, <<"uuid">>).
-define(SCHEME_FIELD_DESCRIPTION, <<"description">>).
-define(SCHEME_FIELD_DONE, <<"done">>).
-define(SCHEME_RULES_ITEM, emodel:compile([
    {?SCHEME_FIELD_UUID, optional, string, ?SCHEME_FIELD_UUID, [], unicode:characters_to_binary(uuid:uuid_to_string(uuid:get_v4_urandom()))},
    {?SCHEME_FIELD_DESCRIPTION, required, string, ?SCHEME_FIELD_DESCRIPTION, [non_empty]},
    {?SCHEME_FIELD_DONE, optional, boolean, ?SCHEME_FIELD_DONE, [], false}
], ?SCHEME_BASIC)).
-define(SCHEME_RULES_ITEM_SELECTOR, emodel:compile([
    {?SCHEME_FIELD_UUID, required, string, ?SCHEME_FIELD_UUID, [non_empty]}
], ?SCHEME_BASIC)).
-define(SCHEME_RULES_ITEM_PAYLOAD, emodel:compile([
    {?SCHEME_FIELD_DESCRIPTION, optional, string, ?SCHEME_FIELD_DESCRIPTION, []},
    {?SCHEME_FIELD_DONE, optional, boolean, ?SCHEME_FIELD_DONE, []}
], ?SCHEME_BASIC)).
-define(DEFAULT_ITEM, #{
    ?SCHEME_FIELD_DONE => false
}).

-spec insert(Item :: todo_item()) ->
    {ok, todo_item()} | {error, any()}.
insert(Item) ->
    case parse_item(Item, ?SCHEME_RULES_ITEM) of
        {error, Reason} -> {error, Reason};
        {ok, PItem} ->
            case todo_items_model_svr:insert(PItem) of
                {{true, _Map}, Doc} ->
                    {ok, NDoc} = parse_item(Doc, ?SCHEME_RULES_ITEM),
                    {ok, NDoc};
                _ ->
                    {error, unable_to_insert}
            end
    end.

-spec update(UUID :: binary(), Item :: todo_item()) ->
    {ok, todo_item()} | {error, any()}.
update(UUID, Item) ->
        Selector = #{ ?SCHEME_FIELD_UUID => UUID },
        {ok, Payload} = parse_item(Item, ?SCHEME_RULES_ITEM_PAYLOAD),

        case todo_items_model_svr:update(Selector, #{ <<"$set">> => Payload }) of
            {true, _} ->
                NDoc = maps:merge(Selector, Payload),
                {ok, NDoc};
            _ ->
                {error, unable_to_update}
        end.

-spec delete(UUID :: binary()) ->
    {ok, map()} | {error, any()}.
delete(UUID) ->
    case todo_items_model_svr:delete(#{ ?SCHEME_FIELD_UUID => UUID }) of
        {true, _} ->
            {ok, UUID};
        _ ->
            {error, unable_to_delete}
    end.

-spec find() -> [todo_item()].
find() -> 
    Items = todo_items_model_svr:find(),
    lists:map(fun(Item) ->
        {ok, PItem} = parse_item(Item, ?SCHEME_RULES_ITEM),
        PItem
    end, Items).

-spec parse_item(Item :: todo_item(), Model :: emodel:pre_model()) ->
    {ok, todo_item()} | {error, any()}.
parse_item(Item, Model) ->
    emodel:from_map(Item, #{}, Model).
