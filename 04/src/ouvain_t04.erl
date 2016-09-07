-module(ouvain_t04).

-export([
         t/0
        ]).

t() ->
    true.

find_divisor(N) ->
    find_divisor(N, 1).

find_divisor(_, Divisor) when N < 10 ->
    Divisor;
find_divisor(N, Divisor) ->
    find_divisor(N div 10, Divisor * 10).

