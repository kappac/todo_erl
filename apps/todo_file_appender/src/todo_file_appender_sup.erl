%%%-------------------------------------------------------------------
%% @doc todo top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(todo_file_appender_sup).

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
        strategy => one_for_all,
        auto_shutdown => any_significant
    },
    ChildSpecs = [#{
        id => todo_file_appender_svr,
        start => {todo_file_appender_svr, start_link, []},
        significant => true,
        restart => transient,
        type => worker
    }],

    todo_transport_http:add_path_handler("/api/file", todo_file_appender_http_handler, []),

    {ok, { SupFlags, ChildSpecs }}.

%% internal functions
