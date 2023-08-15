-module(todo_file_appender_model).

-include("todo_file_appender.hrl").

-export([
    append/1
]).

-define(SCHEME_BASIC, map).
-define(SCHEME_FIELD_FILENAME, <<"filename">>).
-define(SCHEME_FIELD_DATA, <<"data">>).
-define(SCHEME_RULES_APPENDER, emodel:compile([
    {?SCHEME_FIELD_FILENAME, optional, string, ?SCHEME_FIELD_FILENAME, [non_empty]},
    {?SCHEME_FIELD_DATA, required, string, ?SCHEME_FIELD_DATA, [non_empty]}
], ?SCHEME_BASIC)).

-spec append(Input :: todo_appender()) -> ok | {error, any()}.
append(Input) ->
    case parse_input(Input, ?SCHEME_RULES_APPENDER) of
        {error, Reason} -> {error, Reason};
        {ok, #{ ?SCHEME_FIELD_FILENAME := File, ?SCHEME_FIELD_DATA := Data }} ->
            todo_file_appender:file_append(File, Data)
    end.

-spec parse_input(Input :: todo_appender(), Model :: emodel:pre_model()) ->
    {ok, todo_appender()} | {error, any()}.
parse_input(Input, Model) ->
    emodel:from_map(Input, #{}, Model).
