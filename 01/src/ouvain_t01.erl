-module(ouvain_t01).

-export([
         find_sum/2
        ]).

find_sum(Multipliers, Max) when is_integer(Max), Max > 0 ->
    L = [X || X <- lists:seq(1, Max - 1), is_suitable(X, Multipliers)],
    lists:sum(L).

is_suitable(N, Multipliers) ->
    F = fun(Multiplier) ->
                (N rem Multiplier) =:= 0
        end,
    lists:any(F, Multipliers).

