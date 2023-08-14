-module(todo_items_model_sup).

-behaviour(supervisor).

-include_lib("mongodb/include/mongo_types.hrl").

-define(SERVER, ?MODULE).
-define(POOL, model_pool).
-define(COLLECTION, <<"items">>).

%% API
-export([
    find/0,
    start_link/0,
    delete/1,
    init/1
]).

-export([
    insert/1,
    update/2
]).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init(_Args) ->
    SupervisorSpecification = #{ strategy => one_for_one },
    {ok, Pools} = application:get_env(todo_items, pools),

    ChildSpecifications = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        PoolArgs = [
            {name, {local, Name}},
            {worker_module, mc_worker}
            | SizeArgs
        ],
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end, Pools),

    {ok, {SupervisorSpecification, ChildSpecifications}}.

-spec insert(Doc :: bson:document()) -> {{boolean(), map()}, bson:document()};
            (Doc :: map()) -> {{boolean(), map()}, map()};
            (Doc :: list()) -> {{boolean(), map()}, list()}.
insert(Doc) ->
    poolboy:transaction(?POOL, fun(Worker) ->
        mongo_api:insert(Worker, ?COLLECTION, Doc)
    end).

-spec update(Selector :: selector(), Doc :: map() | bson:document()) -> {boolean(), map()}.
update(Selector, Doc) ->
    poolboy:transaction(?POOL, fun(Worker) ->
        mc_worker_api:update(Worker, ?COLLECTION, Selector, Doc)
    end).

-spec delete(Selector :: selector()) -> {boolean(), map()}.
delete(Selector) ->
    poolboy:transaction(?POOL, fun(Worker) ->
        mc_worker_api:delete(Worker, ?COLLECTION, Selector)
    end).

-spec find() -> list().
find() ->
    Query = [],
    poolboy:transaction(?POOL, fun(Worker) ->
        mc_worker_api:find(Worker, Query)
    end).
