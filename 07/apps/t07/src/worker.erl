-module(worker).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    send_signal(),
    lager:debug("~p started", [?MODULE]),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(work, State) ->
    work(),
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

send_signal() ->
    self() ! work.

work() ->
    case task_server:get_number() of
        no_more_numbers ->
            skip;
        Number ->
            lager:debug("get_number, n=~p", [Number]),
            Len = calc_length(Number),
            task_server:finish_number(Number, Len),
            send_signal()
    end.

calc_length(Number) ->
    calc_length(Number, 1).

calc_length(1, Len) ->
    Len;
calc_length(Number, Len) ->
    case num_server:get_cached_len(Number) of
        {ok, Cached_len} ->
            Len + Cached_len - 1;
        error ->
            case is_even(Number) of
                true ->
                    calc_length(Number div 2, Len + 1);
                false ->
                    calc_length(3 * Number + 1, Len + 1)
            end
    end.

is_even(Number) ->
    (Number rem 2) =:= 0.

%% ------------------------------------------------------------------
%% eunit
%% ------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_even_test() ->
    ?assertMatch(true, is_even(0)),
    ?assertMatch(true, is_even(2)),
    ?assertMatch(true, is_even(10)),
    ?assertMatch(true, is_even(12)),
    ?assertMatch(false, is_even(1)),
    ?assertMatch(false, is_even(3)),
    ?assertMatch(false, is_even(11)),
    ok.

calc_length1_test() ->
    meck:new(num_server),
    meck:expect(num_server, get_cached_len, fun(_) -> error end),
    ?assertMatch(10, calc_length(13)),
    meck:unload(num_server),
    ok.

calc_length2_test() ->
    meck:new(num_server),
    meck:expect(num_server, get_cached_len,
                fun
                    (5) -> {ok, 6};
                    (16) -> {ok, 5};
                    (8) -> {ok, 4};
                    (4) -> {ok, 3};
                    (2) -> {ok, 2};
                    (_) -> error
                end),
    ?assertMatch(10, calc_length(13)),
    meck:unload(num_server),
    ok.

-endif.

