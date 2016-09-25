%%% Task keeper.

-module(task_server).

-behaviour(gen_server).

%% API
-export([
         get_number/0,
         finish_number/2,
         start_link/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          finished = not_finished :: not_finished | finished,
          longest_num :: pos_integer(),
          longest_len :: pos_integer(),
          max :: pos_integer()
         }).

%%%===================================================================
%%% API
%%%===================================================================

get_number() ->
    gen_server:call(?SERVER, {get_number, self()}).

finish_number(Number, Length) ->
    gen_server:cast(?SERVER, {finish_number, Number, Length, self()}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Max = application:get_env(t07, max_number, 999999),
    create_tables(),
    self() ! populate_tables,
    lager:debug("~p started", [?MODULE]),
    {ok, #state{max = Max}}.

handle_call({get_number, Pid}, _From, State) ->
    Ref = activate_monitor(Pid),
    Number = get_next_number(Ref),
    lager:debug("get_number, pid=~p, ref=~p, n=~p", [Pid, Ref, Number]),
    {reply, Number, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({finish_number, Number, Length, Pid}, State) ->
    update_cache(Number, Length),
    mark_number_done(Number, Pid),
    State2 = store_result(Number, Length, State),
    lager:info("finish_number, pid=~p, n=~p, len=~p", [Pid, Number, Length]),
    {noreply, State2};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_state, State) ->
    State2 = check_and_update_finish(State),
    log_finish(State2),
    {noreply, State2};
handle_info(populate_tables, #state{max = Max} = State) ->
    populate_tables(Max),
    {noreply, State};
handle_info({'DOWN', Ref, _, Pid, _}, State) ->
    mark_number_free(Ref, Pid),
    lager:debug("worker down, pid=~p, ref=~p", [Pid, Ref]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_next_number(Ref) ->
    case get_next_number() of
        no_more_numbers = Err ->
            prepare_check_state(),
            Err;
        N ->
            mark_number_busy(N, Ref),
            N
    end.

store_result(Number, Length, #state{longest_len = undefined} = State) ->
    update_state(Number, Length, State);
store_result(Number, Length, #state{longest_len = L} = State) when Length > L ->
    update_state(Number, Length, State);
store_result(_, _, State) ->
    State.

update_state(Number, Length, State) ->
    Finished = get_finish_flag(),
    State#state{longest_num = Number,
                longest_len = Length,
                finished = Finished}.

check_and_update_finish(State) ->
    Finished = get_finish_flag(),
    State#state{finished = Finished}.

log_finish(#state{longest_num = N, longest_len = L, finished = finished}) ->
    lager:info("finished, n=~p, len=~p~n", [N, L]);
log_finish(_) ->
    ok.

get_finish_flag() ->
    S1 = ets:info(busy_number_tab(), size),
    S2 = ets:info(free_number_tab(), size),
    case S1 + S2 of
        0 ->
            finished;
        _ ->
            not_finished
    end.

activate_monitor(Pid) ->
    case is_monitored(Pid) of
        true ->
            get_monitor_ref(Pid);
        false ->
            Ref = erlang:monitor(process, Pid),
            store_monitor_ref(Pid, Ref),
            Ref
    end.

is_monitored(Pid) ->
    case ets:lookup(pid_to_monitor_tab(), Pid) of
        [{Pid, _}] ->
            true;
        _ ->
            false
    end.

get_monitor_ref(Pid) ->
    [{Pid, Ref}] = ets:lookup(pid_to_monitor_tab(), Pid),
    Ref.

store_monitor_ref(Pid, Ref) ->
    ets:insert(pid_to_monitor_tab(), {Pid, Ref}).

clear_monitor_ref(Pid, Ref) ->
    case ets:lookup(pid_to_monitor_tab(), Pid) of
        [{Pid, Ref}] ->
            ets:delete(pid_to_monitor_tab(), Pid);
        _ ->
            skip
    end.

get_next_number() ->
    case ets:first(free_number_tab()) of
        '$end_of_table' ->
            no_more_numbers;
        N ->
            N
    end.

mark_number_busy(N, Ref) ->
    ets:delete(free_number_tab(), N),
    ets:insert(busy_number_tab(), {N, Ref}),
    ets:insert(monitor_to_number_tab(), {Ref, N}).

mark_number_free(Ref, Pid) ->
    clear_monitor_ref(Pid, Ref),
    case ets:lookup(monitor_to_number_tab(), Ref) of
        [{Ref, N}] ->
            ets:delete(monitor_to_number_tab(), Ref),
            ets:delete(busy_number_tab(), N),
            ets:insert(free_number_tab(), {N});
        _ ->
            skip
    end.

mark_number_done(Number, Pid) ->
    Ref = get_monitor_ref(Pid),
    case ets:lookup(monitor_to_number_tab(), Ref) of
        [{Ref, Number}] ->
            ets:delete(monitor_to_number_tab(), Ref),
            ets:delete(busy_number_tab(), Number);
        _ ->
            skip
    end.

update_cache(Number, Length) ->
    num_server:store(Number, Length).

pid_to_monitor_tab() ->
    pid_to_monitor.

monitor_to_number_tab() ->
    monitor_to_number.

busy_number_tab() ->
    busy_numbers.

free_number_tab() ->
    free_numbers.

create_tables() ->
    ets:new(pid_to_monitor_tab(), [named_table]),
    ets:new(monitor_to_number_tab(), [named_table]),
    ets:new(busy_number_tab(), [named_table]),
    ets:new(free_number_tab(), [named_table, ordered_set]).

populate_tables(Max) ->
    [ets:insert(free_number_tab(), {X}) || X <- lists:seq(1, Max)],
    ok.

prepare_check_state() ->
    self() ! check_state.

