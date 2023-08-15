-module(todo_file_appender).

-export([
    file_append/2
]).

-spec file_append(File :: file:name_all() | iodata(), Data :: iodata()) -> ok.
file_append(File, Data) ->
    gen_server:call(todo_file_appender_svr, {append, File, Data}).
