-module(ouvain_t04).

-export([
         is_palindrome/1
        ]).

is_palindrome(N) ->
    is_palindrome(N, []).

is_palindrome(N, Acc) when N < 10 ->
    compare([N | Acc]);
is_palindrome(N, Acc) ->
    Digit = N rem 10,
    N2 = N div 10,
    is_palindrome(N2, [Digit | Acc]).

compare(Digits) ->
    Normal = lists:reverse(Digits),
    Normal =:= Digits.

