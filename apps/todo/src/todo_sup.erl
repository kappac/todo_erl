%%%-------------------------------------------------------------------
%% @doc todo top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(todo_sup).

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
    ChildSpecs = [
        #{
            id => todo_transport_http,
            start => {todo_transport_http_sup, start_link, []},
            significant => true,
            restart => transient,
            type => supervisor
        },
        #{
            id => todo_items_sup,
            start => {todo_items_sup, start_link, []},
            significant => true,
            restart => transient,
            type => supervisor
        },
        #{
            id => todo_file_appender_sup,
            start => {todo_file_appender_sup, start_link, []},
            restart => transient,
            type => supervisor
        }
    ],

    {ok, { SupFlags, ChildSpecs }}.

%% internal functions
