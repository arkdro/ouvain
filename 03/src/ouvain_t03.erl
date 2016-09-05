%%
%% 25% own ideas, 75% copy/paste from stackoverflow
%%

-module(ouvain_t03).

-export([
         fac/1,
         fac_max/1
        ]).

fac_max(N) ->
    L = fac(N),
    hd(lists:reverse(lists:usort(L))).

fac(N) ->
    fac(N, 2, []).

fac(N, D, Acc) when N > 1 ->
    {N2, Acc2} = add_small_factors(N, D, Acc),
    D2 = D + 1,
    case too_big(D2, N2) of
        true when N2 > 1 ->
            add_factor(N2, Acc2);
        true ->
            Acc2;
        false ->
            fac(N2, D2, Acc2)
    end;
fac(_, _, Acc) ->
    Acc.

add_small_factors(N, D, Acc) ->
    case N rem D of
        0 ->
            Acc2 = add_factor(D, Acc),
            add_small_factors(N div D, D, Acc2);
        _ ->
            {N, Acc}
    end.

add_factor(D, Acc) ->
    [D | Acc].

too_big(D, N) ->
    D * D > N.

