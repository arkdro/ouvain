-module(ouvain_t04).

-export([
         find/1,
         find2/1,
         is_palindrome/1
        ]).

%% O(N^2)
find(Len) ->
    Start = get_start(Len),
    L = [{N1 * N2, N1, N2} || N1 <- lists:seq(Start, 1, -1),
                              N2 <- lists:seq(Start, 1, -1),
                              is_palindrome(N1 * N2)],
    get_biggest_result(L).

%% O(N^2), but little faster
find2(Len) ->
    Params = params(),
    find2(Len, Params).

find2(_, []) ->
    not_found;
find2(Len, [{Digit, Digits} | T]) ->
    Min1 = get_min_n(Digit, Len),
    Min2 = Min1,
    Max1 = get_start(Digit, Len),
    Max2 = Max1,
    {D1, D2} = get_next_digits(Digits),
    Init1 = get_initial_num(Max1, D1),
    Init2 = get_initial_num(Max2, D2),
    Acc = find2(Init1, Init2, Min1, Min2, Max1, Max2, Digits, []),
    case has_result(Acc) of
        true ->
            get_biggest_result(Acc);
        false ->
            find2(Len, T)
    end.

get_biggest_result(L) ->
    hd(lists:reverse(lists:sort(L))).

has_result([]) ->
    false;
has_result([_|_]) ->
    true.

find2(_N1, _N2, _Min1, _Min2, _Max1, _Max2, [], _Acc) ->
    error(should_not_happen);
find2(N1, N2, Min1, Min2, Max1, Max2, [{D1, _} | _] = Digits, Acc)
  when N1 < Min1 ->
    N1_2 = get_initial_num(Max1, D1),
    N2_2 = get_next_num(N2),
    find2(N1_2, N2_2, Min1, Min2, Max1, Max2, Digits, Acc);
find2(_, N2, _, Min2, _, _, [_], Acc) when N2 < Min2 ->
    Acc;
find2(_, N2, Min1, Min2, Max1, Max2, [_ | T], Acc)
  when N2 < Min2 ->
    {D1, D2} = get_next_digits(T),
    Init1 = get_initial_num(Max1, D1),
    Init2 = get_initial_num(Max2, D2),
    find2(Init1, Init2, Min1, Min2, Max1, Max2, T, Acc);
find2(N1, N2, Min1, Min2, Max1, Max2, Digits, Acc) ->
    X = N1 * N2,
    Acc2 = add_if_palindrome(X, N1, N2, Acc),
    N1_2 = get_next_num(N1),
    find2(N1_2, N2, Min1, Min2, Max1, Max2, Digits, Acc2).

get_next_digits([Digits | _]) ->
    Digits.

get_initial_num(Max, D) ->
    Rem = Max rem 10,
    Max - Rem + D.

get_next_num(N) ->
    N - 10.

add_if_palindrome(X, N1, N2, Acc) ->
    case is_palindrome(X) of
        true ->
            [{X, N1, N2} | Acc];
        false ->
            Acc
    end.

params() ->
    [
     {9, [{1,9}, {3,3}]},
     {8, [{1,8}, {2,4}]},
     {7, [{1,7}]},
     {6, [{1,6}, {2,3}]},
     {5, [{1,5}]},
     {4, [{1,4}, {2,2}]},
     {3, [{1,3}]},
     {2, [{1,2}]},
     {1, [{1,1}]}
    ].

get_start(Len) ->
    round(math:pow(10, Len)) - 1.

calc_multiplier(Len) ->
    round(math:pow(10, Len - 1)).

get_min_n(Digit, Len) ->
    Multiplier = calc_multiplier(Len),
    Digit * Multiplier.

get_start(Digit, 1) ->
    Digit;
get_start(Digit, Len) ->
    Head = get_min_n(Digit, Len),
    Multiplier = calc_multiplier(Len),
    Tail = Multiplier - 1,
    Head + Tail.

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

get_start_test() ->
    ?assertMatch(999, get_start(9, 3)),
    ?assertMatch(599, get_start(5, 3)),
    ?assertMatch(199, get_start(1, 3)),
    ?assertMatch(99, get_start(9, 2)),
    ?assertMatch(59, get_start(5, 2)),
    ?assertMatch(19, get_start(1, 2)),
    ?assertMatch(9, get_start(9, 1)),
    ?assertMatch(5, get_start(5, 1)),
    ?assertMatch(1, get_start(1, 1)),
    ok.

-endif.

