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

%% ------------------------------------------------------------------
%% eunit
%% ------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_palindrom_test() ->
    ?assertMatch(true, compare([1,2,3,4,3,2,1])),
    ?assertMatch(true, compare([1,2,3,4,4,3,2,1])),
    ?assertMatch(true, compare([4])),
    ?assertMatch(true, compare([4,4])),
    ?assertMatch(true, compare([1,1])),
    ?assertMatch(false, compare([1,2,3,4,4,3,2,1,0])),
    ?assertMatch(false, compare([1,0])),
    ?assertMatch(false, compare([2,1])),
    ok.

-endif.

