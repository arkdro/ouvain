-module(t02_SUITE).

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
                 fib
                ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

fib(_Config) ->
    ?assertMatch(10, ouvain_t02:fib(10)),
    ?assertMatch(44, ouvain_t02:fib(100)),
    ?assertMatch(4613732, ouvain_t02:fib(4000000)),
    ok.

