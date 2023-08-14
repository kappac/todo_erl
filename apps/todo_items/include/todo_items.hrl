-ifndef(TODO_ITEMS).
-define(TODO_ITEMS, true).

-type todo_item_id() :: non_neg_integer() | undefined.
-type todo_item_description() :: unicode:binary().
-type todo_is_done() :: boolean() | undefined.
-type todo_item() :: #{
    uuid => todo_item_id(),
    description => todo_item_description(),
    is_done => todo_is_done()
}.

-endif.
