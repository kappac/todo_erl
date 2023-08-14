-module(todo_transport_http_svr).

-behaviour(gen_server).

%% API
-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).
-define(COWBOY_REF, list_to_atom(lists:flatten(io_lib:format("~p_~p", [?SERVER, "cowboy"])))).
-define(COWBOY_DISPATCH, list_to_atom(lists:flatten(io_lib:format("~p_~p_~p", [?SERVER, "cowboy", "dispatch"])))).
-define(DEFAULT_INIT_OPTS, #{
    host => '_',
    port => 8080
}).

-type init_opts() :: #{
    host => string() | '_',
    port => non_neg_integer()
}.
-type routes() :: cowboy_router:routes().
-type state() :: #{
    opts => init_opts(),
    routes => routes()
}.

-spec start_link(Opts :: init_opts())
    -> gen:start_ret().
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

-spec init(Opts :: init_opts())
    -> {ok, state()}.
init(Opts) ->
    EOpts = normalize_opts(Opts),
    NState = #{
        opts => EOpts,
        routes => [{maps:get(host, EOpts), []}]
    },

    init_cowboy(),

    {ok, NState}.

handle_call(
    {add_path_handler, Path, Module, Opts},
    _From,
    #{ routes := [{Host, Routes}] } = State
) ->
    Pred = fun ({RPath, _, _}) -> Path =:= RPath end,
    HasPath = lists:any(Pred, Routes),

    {Result, ERoutes, NState} = if
        HasPath ->
            {{error, already_exists}, State};
        true ->
            NRoutes = [{Host, [{Path, Module, Opts} | Routes]}],
            UState = State#{ routes := NRoutes },

            {ok, NRoutes, UState}
    end,

    update_dispatch(ERoutes),

    {reply, Result, NState};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({init_cowboy}, #{ opts := #{ port := Port }, routes := Routes } = State) ->
    update_dispatch(Routes),
    {ok, _} = cowboy:start_clear(
        ?COWBOY_REF,
        [{port, Port}],
        #{ env => #{ dispatch => {persistent_term, ?COWBOY_DISPATCH} } }
    ),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    cowboy:stop_listener(?COWBOY_REF),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal

-spec init_cowboy() -> ok.
init_cowboy() ->
    gen_server:cast(?SERVER, {init_cowboy}).

-spec normalize_opts(Opts :: init_opts())
    -> init_opts().
normalize_opts(Opts) ->
    maps:merge(?DEFAULT_INIT_OPTS, Opts).

-spec update_dispatch(Routes :: cowboy_router:routes())
    -> ok.
update_dispatch(Routes) ->
    Dispatch = cowboy_router:compile(Routes),
    persistent_term:put(?COWBOY_DISPATCH, Dispatch).
