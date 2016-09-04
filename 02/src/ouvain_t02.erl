-module(ouvain_t02).

-export([
         fib/1
        ]).

fib(Max) when Max < 0 ->
    error(badarg);
fib(Max) ->
    fib(Max, fun calc_new_acc/3).

fib(Max, Fun) ->
    fib(0, 1, Max, 0, Fun).

fib(_, Cur2, Max, Acc, _Fun) when Cur2 > Max ->
    Acc;
fib(Cur1, Cur2, Max, Acc, Fun) ->
    Next = Cur1 + Cur2,
    New = Fun(Cur2, Max, Acc),
    fib(Cur2, Next, Max, New, Fun).

calc_new_acc(Cur, Max, Acc) when Cur < Max,
                                 (Cur rem 2) =:= 0 ->
    Acc + Cur;
calc_new_acc(_, _, Acc) ->
    Acc.

%% ------------------------------------------------------------------
%% eunit
%% ------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

fib_test() ->
    Exp = [89, 55, 34, 21, 13, 8, 5, 3, 2, 1],
    F = fun(Cur, _, Acc) ->
                [Cur | Acc]
        end,
    Act = fib(1, 1, 100, [], F),
    %% ?debugFmt("act: ~p", [Act]),
    ?assertMatch(Exp, Act),
    ok.

fib_2_test() ->
    Exp = 4613732,
    F = fun(Cur, _, Acc) ->
                [Cur | Acc]
        end,
    L = fib(1, 1, 4000000, [], F),
    %% ?debugFmt("L: ~p", [L]),
    L2 = [X || X <- L, (X rem 2) =:= 0],
    Act = lists:sum(L2),
    %% ?debugFmt("act: ~p", [Act]),
    ?assertMatch(Exp, Act),
    ok.

-endif.

