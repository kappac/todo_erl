-module(todo_transport_http).

-export([
    add_path_handler/3,
    remove_path_handler/1
]).

-type path() :: '_' | iodata().

-spec add_path_handler(Path :: path(), Module :: module(), Opts :: any())
    -> ok | {error, already_exists}.
add_path_handler(Path, Module, Opts) ->
    gen_server:call(todo_transport_http_svr, {add_path_handler, Path, Module, Opts}).

-spec remove_path_handler(Path :: path())
    -> ok.
remove_path_handler(Path) ->
    gen_server:call(todo_transport_http_svr, {remove_path_handler, Path}).
