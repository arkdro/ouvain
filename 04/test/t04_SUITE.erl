-module(t04_SUITE).

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
                 test_palindrome
                ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

test_palindrome(_Config) ->
    ?assertMatch(true, ouvain_t04:is_palindrome(1234321)),
    ?assertMatch(true, ouvain_t04:is_palindrome(12344321)),
    ?assertMatch(true, ouvain_t04:is_palindrome(4)),
    ?assertMatch(true, ouvain_t04:is_palindrome(44)),
    ?assertMatch(true, ouvain_t04:is_palindrome(11)),
    ?assertMatch(false, ouvain_t04:is_palindrome(123443210)),
    ?assertMatch(false, ouvain_t04:is_palindrome(10)),
    ?assertMatch(false, ouvain_t04:is_palindrome(21)),
    ok.

