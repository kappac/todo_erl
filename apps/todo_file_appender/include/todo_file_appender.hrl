-ifndef(todo_file_appender).
-define(todo_file_appender, true).

-type todo_appender_file() :: unicode:binary().
-type todo_appender_data() :: unicode:binary().
-type todo_appender() :: #{
    file => todo_appender_file(),
    data => todo_appender_data()
}.

-endif.
