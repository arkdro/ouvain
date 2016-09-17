-module(t06_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    start_cowboy(),
    t06_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% Internal
%% ===================================================================

start_cowboy() ->
    Routes =
        [
         {'_', [
                {"/capture", capture_handler, []}
               ]}
        ],
    Dispatch = cowboy_router:compile(Routes),
    Opts = [
            {env, [{dispatch, Dispatch}]}
           ],
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], Opts).

