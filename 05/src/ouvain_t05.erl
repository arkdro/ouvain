-module(ouvain_t05).

-export([
         find2/1,
         find2/2,
         find/1,
         find/2
        ]).

-spec find2(pos_integer(), pos_integer()) -> {ok, pos_integer()}.

find2(Begin, End) ->
    find2(lists:seq(Begin, End)).

-spec find2([pos_integer()]) -> {ok, pos_integer()}.

find2(Numbers) ->
    {Status, Acc} = find2(primes(), Numbers, []),
    Lcm = lists:foldl(fun erlang:'*'/2, 1, Acc),
    {Status, Lcm}.

find2([], _, Acc) ->
    {error, Acc};
find2([Divisor | T] = Divs, Numbers, Acc) ->
    case get_next_row(Divisor, Numbers) of
        done ->
            {ok, [Divisor | Acc]};
        {updated, Row} ->
            find2(Divs, Row, [Divisor | Acc]);
        not_updated ->
            find2(T, Numbers, Acc)
    end.

%% find/1, find/2 - SLOW!!!
-spec find(pos_integer(), pos_integer()) -> pos_integer().

find(Begin, End) ->
    find(lists:seq(Begin, End)).

-spec find([pos_integer()]) -> pos_integer().

find(Numbers) ->
    find_priv(Numbers, Numbers).

find_priv(Orig, Numbers) ->
    case find_index_of_min(Numbers) of
        {all_equal, N} ->
            N;
        Idx ->
            Numbers2 = add_min(Idx, Orig, Numbers),
            find_priv(Orig, Numbers2)
    end.

find_index_of_min(L) ->
    find_index_of_min(L, 1, stub, -1, 1).

find_index_of_min([], _, N, N, _) ->
    {all_equal, N};
find_index_of_min([], _, _, _, Idx_of_min) ->
    Idx_of_min;
find_index_of_min([H | T], Idx, stub, Max, _) ->
    find_index_of_min(T, Idx + 1, H, Max, Idx);
find_index_of_min([H | T], Idx, Min, Max, _) when H < Min ->
    find_index_of_min(T, Idx + 1, H, Max, Idx);
find_index_of_min([H | T], Idx, Min, Max, Idx_of_min) when H > Max ->
    find_index_of_min(T, Idx + 1, Min, H, Idx_of_min);
find_index_of_min([_ | T], Idx, Min, Max, Idx_of_min) ->
    find_index_of_min(T, Idx + 1, Min, Max, Idx_of_min).

add_min(Idx, Orig, Numbers) ->
    N = lists:nth(Idx, Orig),
    {L1, [H | T]} = lists:split(Idx - 1, Numbers),
    H2 = H + N,
    L1 ++ [H2 | T].

primes() ->
    [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97].

get_next_row(Divisor, Numbers) ->
    get_next_row(Divisor, Numbers, [], -1, not_updated).

get_next_row(_, [], _, 1, _) ->
    done;
get_next_row(_, [], Row, _, updated) ->
    {updated, Row};
get_next_row(_, [], _, _, not_updated) ->
    not_updated;
get_next_row(Divisor, [1 | T], Row, Max, Status) ->
    get_next_row(Divisor, T, [1 | Row], Max, Status);
get_next_row(Divisor, [H | T], Row, Max, Status) ->
    case split_number(Divisor, H) of
        {P, 0} ->
            get_next_row(Divisor, T, [P | Row], max(P, Max), updated);
        _ ->
            get_next_row(Divisor, T, [H | Row], max(H, Max), Status)
    end.

split_number(Divisor, N) ->
    {N div Divisor,
     N rem Divisor}.

choose_number({_, {P, 0}}) ->
    P;
choose_number({N, {_, _}}) ->
    N.

%% ------------------------------------------------------------------
%% eunit
%% ------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

find_index_of_min_test() ->
    ?assertMatch(1, find_index_of_min([1])),
    ?assertMatch(1, find_index_of_min([1, 2, 3, 4, 3, 2, 1])),
    ?assertMatch(5, find_index_of_min([3, 4, 3, 2, 1])),
    ?assertMatch(4, find_index_of_min([3, 4, 3, 1, 1])),
    ?assertMatch({all_equal, 3}, find_index_of_min([3, 3, 3, 3, 3])),
    ok.

-endif.

