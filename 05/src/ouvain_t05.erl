-module(ouvain_t05).

-export([
         find/1,
         find/2
        ]).

find(Begin, End) ->
    find(lists:seq(Begin, End)).

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

