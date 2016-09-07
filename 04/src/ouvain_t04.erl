-module(ouvain_t04).

-export([
         t/0
        ]).

t() ->
    true.

is_palindrome(N, _Divisor) when N >= 0, N < 10 ->
    true;
is_palindrome(N, Divisor) ->
    Beg = N div Divisor,
    End = N rem 10,
    case Beg =:= End of
        true ->
            N2 = N div 10,
            Divisor2 = Divisor div 10,
            is_palindrome(N2, Divisor2);
        false ->
            false
    end.

find_divisor(N) ->
    find_divisor(N, 1).

find_divisor(_, Divisor) when N < 10 ->
    Divisor;
find_divisor(N, Divisor) ->
    find_divisor(N div 10, Divisor * 10).

