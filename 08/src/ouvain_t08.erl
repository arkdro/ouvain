-module(ouvain_t08).

-export([
         validate/1
        ]).

validate({gtin, Gtin}) ->
    Str = make_string(Gtin),
    Validators = [
                  fun validate_length/1,
                  fun validate_check_digit/1
                 ],
    validate(Str, Validators).

validate(_, []) ->
    ok;
validate(Str, [H | T]) ->
    case H(Str) of
        ok ->
            validate(Str, T);
        {error, _} = Error ->
            Error
    end.

validate_check_digit(Str) ->
    [Check | Reversed] = lists:reverse(Str),
    Calculated = calc_check_digit(Reversed),
    Check_num = list_to_integer([Check]),
    case Calculated of
        Check_num ->
            ok;
        _ ->
            {error, wrong_check_digit}
    end.

calc_check_digit(Str) ->
    calc_check_digit(Str, 3, 1, 0).

calc_check_digit([], _, _, Acc) ->
    (10 - (Acc rem 10)) rem 10;
calc_check_digit([H | T], Mult, Next, Acc) ->
    Num = list_to_integer([H]),
    Acc2 = Acc + Num * Mult,
    calc_check_digit(T, Next, Mult, Acc2).

validate_length(Str) ->
    case length(Str) of
        14 ->
            ok;
        _ ->
            {error, invalid_length}
    end.

make_string(Data) ->
    unicode:characters_to_list(Data).

%% ------------------------------------------------------------------
%% eunit
%% ------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

validate_test() ->
    ?assertMatch(0, calc_check_digit("", 1, 3, 0)),
    ?assertMatch(9, calc_check_digit("", 1, 3, 1)),
    ?assertMatch(1, calc_check_digit("", 1, 3, 9)),
    ?assertMatch(5, calc_check_digit("", 1, 3, 5)),
    ?assertMatch(6, calc_check_digit("1", 3, 1, 11)),
    ?assertMatch(3, calc_check_digit(lists:reverse("629104150021"))),
    ok.

-endif.

