-module(ouvain_t04).

-export([
         find/1,
         find2/1,
         is_palindrome/1
        ]).

find(Len) ->
    Start = get_start(Len),
    L = [{N1 * N2, N1, N2} || N1 <- lists:seq(Start, 1, -1),
                              N2 <- lists:seq(Start, 1, -1),
                              is_palindrome(N1 * N2)],
    hd(lists:reverse(lists:sort(L))).

find2(Len) ->
    Max = get_start(Len),
    Params = params(),
    {Digit, Deltas} = hd(Params),
    Min = build_min(Digit, Len),
    N1 = Max,
    N2 = Max,
    X = N1 * N2,
    Acc = build_nums(X, N1, N2, Min, Max, Max, Deltas, []),
    case has_result(Acc) of
        true ->
            stub;
        false ->
            find(Len)
    end.

has_result([]) ->
    false;
has_result([_|_]) ->
    true.

%% only for 9xx
build_nums(_X, _N1, _N2, _Min, _Max1, _Max2, [], Acc) ->
    Acc;
build_nums(X, N1, N2, Min, Max1, Max2, [{D1, D2} | T] = Deltas, Acc) ->
    case too_small(X, Min) of
        true ->
            N1_2 = Max1,
            N2_2 = N2 - D2,
            X2 = N1_2 * N2_2,
            case too_small(X2, Min) of
                true ->
                    build_nums(Max1 * Max2, Min, Max1, Max2, Max1, Max2, T, Acc);
                false ->
                    X2 = N1_2 * N2_2,
                    build_nums(X2, N1_2, N2_2, Min, Max1, Max2, Deltas, Acc)
            end;
        false ->
            Acc2 = add_if_palindrome(X, Acc),
            N1_2 = N1 - D1,
            X2 = N1_2 * N2,
            build_nums(X2, N1_2, N2, Min, Max1, Max2, Deltas, Acc2)
    end.

build_min(Digit, Len) ->
    round(Digit * math:pow(10, 2 * Len - 1)).

add_if_palindrome(X, Acc) ->
    case is_palindrome(X) of
        true ->
            [X | Acc];
        false ->
            Acc
    end.

too_small(N, Min) ->
    N < Min.

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

