-module(ouvain_t08).

-export([
         validate/1
        ]).

validate(Gtin) ->
    Str = make_string(Gtin),
    Validators = [
                  fun validate_length/1
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

%    Reversed = lists:reverse(Str),

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
    ?assertMatch({error, invalid_length}, validate(<<>>)),
    ?assertMatch({error, invalid_length}, validate(<<"12341234123412341234">>)),
    ?assertMatch({error, invalid_length}, validate("")),
    ?assertMatch({error, invalid_length}, validate("12")),
    %% ?assertMatch({error, wrong_check_digit}, validate("12345678901234")),
    ok.

-endif.

