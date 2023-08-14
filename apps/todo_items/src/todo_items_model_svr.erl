-module(todo_items_model_svr).

-behaviour(gen_server).

-include("todo_items.hrl").

%% API
-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    delete/1,
    insert/1,
    update/2,
    find/0
]).

-define(SERVER, ?MODULE).
-define(MNG_POOL_NAME, mng_pool).
-define(MNG_TOPOLOGY, mng_topology).
-define(MNG_SEED, "mongo:27017").
-define(MNG_DATABASE, <<"todos">>).
-define(MNG_COLLECTION, <<"items">>).
-define(MNG_LOGIN, <<"root">>).
-define(MNG_PASSWORD, <<"password">>).
-define(MNG_POOL_SIZE, 10).
-define(MNG_MAX_OVERFLOW, 20).

-record(state, { mng_topology }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_Args) ->
    COpts = [
        {name, ?MNG_POOL_NAME},
        {register, ?MNG_TOPOLOGY},
        {pool_size, ?MNG_POOL_SIZE},
        {max_overflow, ?MNG_MAX_OVERFLOW}
    ],
    WOpts = [
        {database, ?MNG_DATABASE},
        {login, ?MNG_LOGIN},
        {password, ?MNG_PASSWORD}
    ],

    case mongoc:connect(?MNG_SEED, COpts, WOpts) of
        {ok, Pid} ->
            {ok, #state{ mng_topology = Pid }};
        ignore ->
            {ok, #state{}};
        {error, Reason} ->
            {error, Reason}
    end.

handle_call({insert, Doc}, _From, State) ->
    Res = mongo_api:insert(State#state.mng_topology, ?MNG_COLLECTION, Doc),
    {reply, Res, State};
handle_call({update, Selector, Doc}, _From, State) ->
    Res = mongo_api:update(State#state.mng_topology, ?MNG_COLLECTION, Selector, Doc, #{}),
    {reply, Res, State};
handle_call({delete, Selector}, _From, State) ->
    Res = mongo_api:delete(State#state.mng_topology, ?MNG_COLLECTION, Selector),
    {reply, Res, State};
handle_call({find}, _From, State) ->
    Res = case mongo_api:find(State#state.mng_topology, ?MNG_COLLECTION, #{}, #{}) of
        {ok, Cursor} ->
            case mc_cursor:rest(Cursor) of
                error -> [];
                List -> List
            end;
        _ -> []
    end,
    {reply, Res, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    mongoc:disconnect(State#state.mng_topology).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec insert(Item :: todo_item()) -> {{boolean(), map()}, list()} | {error, any()}.
insert(Item) ->
    gen_server:call(?SERVER, {insert, Item}).

-spec update(Selector :: map(), Item :: todo_item()) -> {boolean(), map()} | {error, any()}.
update(Selector, Item) ->
    gen_server:call(?SERVER, {update, Selector, Item}).

-spec delete(Selector :: map()) -> {boolean(), map()} | {error, any()}.
delete(Selector) ->
    gen_server:call(?SERVER, {delete, Selector}).

-spec find() -> [todo_item()].
find() ->
    gen_server:call(?SERVER, {find}).
