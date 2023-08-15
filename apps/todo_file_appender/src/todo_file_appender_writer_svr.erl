-module(todo_file_appender_writer_svr).

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

-define(CLOSE_TIMEOUT, 10 * 1000).

-record(state, { file }).

-type init_args() :: {
    File :: file:name_all() | iodata()
}.

-spec start_link(Args :: init_args()) ->
    gen:start_ret().
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

-spec init(Args :: init_args()) ->
    {ok, #state{}} | {stop, {file_open, Reason :: any()}}.
init({ File }) ->
    process_flag(trap_exit, true),

    case file:open(File, [append]) of
        {ok, F} -> {ok, #state{ file = F }};
        {error, Reason} -> {stop, {file_open, Reason}}
    end.

handle_call({append, Data}, _From, State) ->
    ok = io:fwrite(State#state.file, "~s~n", [Data]),
    {reply, ok, State, ?CLOSE_TIMEOUT};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({stop}, State) ->
    ok = file:close(State#state.file),
    {stop, normal, #state{}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _SenderID, _Reason}, State) ->
    stop(),
    {noreply, State};
handle_info(timeout, State) ->
    stop(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal
stop() ->
    gen_server:cast(self(), {stop}).
