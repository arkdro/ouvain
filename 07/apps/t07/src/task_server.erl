%%% Task keeper.

-module(task_server).

-behaviour(gen_server).

%% API
-export([
         get_number/0,
         finish_number/1,
         start_link/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          cur = 1 :: pos_integer(),
          max :: pos_integer()
         }).

%%%===================================================================
%%% API
%%%===================================================================

get_number() ->
    erlang:error(not_implemented).

finish_number(Number) ->
    erlang:error(not_implemented).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Max = application:get_env(t07, max_number, 999999),
    {ok, #state{max = Max}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

