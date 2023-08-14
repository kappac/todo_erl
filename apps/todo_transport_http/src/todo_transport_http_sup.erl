%%%-------------------------------------------------------------------
%% @doc todo top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(todo_transport_http_sup).

-behaviour(supervisor).

-export([
    start_link/0,
    init/1
]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        auto_shutdown => any_significant
    },
    ChildSpecs = [#{
        id => todo_transport_http_svr,
        start => {todo_transport_http_svr, start_link, [#{}]},
        significant => true,
        restart => transient,
        type => worker
    }],

    {ok, { SupFlags, ChildSpecs }}.

%% internal functions
