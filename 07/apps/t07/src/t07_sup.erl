-module(t07_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Num_server = ?CHILD(num_server, worker),
    Task_server = ?CHILD(task_server, worker),
    N = application:get_env(t07, max_workers, 1),
    Workers = build_worker_specs(N),
    Children = [Num_server, Task_server | Workers],
    {ok, { {one_for_one, 5, 10}, Children} }.

build_worker_specs(N) ->
    [build_one_worker_spec(X) || X <- lists:seq(1, N)].

build_one_worker_spec(N) ->
    Mod = worker,
    Id = {Mod, N},
    Mfa = {Mod, start_link, []},
    {Id, Mfa, permanent, brutal_kill, worker, [Mod]}.

