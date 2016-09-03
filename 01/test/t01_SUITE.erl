-module(t01_SUITE).

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
                 find_sum1
                ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

find_sum1(_Config) ->
    ?assertMatch(23, ouvain_t01:find_sum([3, 5], 10)),
    ?assertMatch(233168, ouvain_t01:find_sum([3, 5], 1000)),
    ok.

