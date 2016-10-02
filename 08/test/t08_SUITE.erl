-module(t08_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(ASSERT, true).

-compile([export_all]).

suite() ->
    [
     {timetrap, {seconds, 180}}
    ].

all() ->
    [
     {group, all}
    ].

groups() ->
    [
     {all, [], [
                {group, read}
               ]},
     {read, [], [
                 test_validate
                ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

test_validate(_Config) ->
    ?assertMatch({error, invalid_length}, ouvain_t08:validate({gtin, <<>>})),
    ?assertMatch({error, invalid_length}, ouvain_t08:validate({gtin, <<"12341234123412341234">>})),
    ?assertMatch({error, invalid_length}, ouvain_t08:validate({gtin, ""})),
    ?assertMatch({error, invalid_length}, ouvain_t08:validate({gtin, "12"})),
    ?assertMatch({error, wrong_check_digit}, ouvain_t08:validate({gtin, "12345678901234"})),
    ?assertMatch(ok, ouvain_t08:validate({gtin, "12345678901231"})),
    ?assertMatch(ok, ouvain_t08:validate({gtin, "06291041500213"})),
    ok.

