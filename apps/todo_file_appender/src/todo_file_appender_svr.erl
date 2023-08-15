-module(todo_file_appender_svr).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).
-define(DIRECTORY, "/files/").

-record(state, { writers }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{ writers = dict:new() }}.

handle_call({append, File, Data}, _From, State) ->
    {Status, Writer} = case dict:find(File, State#state.writers) of
        {ok, [EWriter]} -> {existing, EWriter};
        error -> 
            StartArgs = {
                get_file_name(File)
            },
            {ok, Pid} = todo_file_appender_writer_svr:start_link(StartArgs),
            {new, Pid}
    end,

    NState = if
        Status =:= new -> #state{
            writers = dict:append(File, Writer, State#state.writers)
        };
        true -> State
    end,

    gen_server:call(Writer, {append, Data}),

    {reply, ok, NState};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Sender, _Reason}, State) ->
    NState = #state{
        writers = dict:filter(
            fun(_File, [Writer]) -> Writer /= Sender end,
            State#state.writers
        )
    },
    {noreply, NState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal
get_file_name(File) ->
    io_lib:format("~s~s", [?DIRECTORY, File]).
