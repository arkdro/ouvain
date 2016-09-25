%%% Cache for {start_number, sequence_length}.

-module(num_server).

-behaviour(gen_server).

%% API
-export([
         get_cached_len/1,
         store/2,
         start_link/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

get_cached_len(Number) ->
    case ets:lookup(?TAB, Number) of
        [{Number, Len}] ->
            {ok, Len};
        [] ->
            error
    end.

store(Start, Len) ->
    gen_server:cast(?SERVER, {store, Start, Len}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ets:new(?TAB, [named_table]),
    lager:debug("~p started", [?MODULE]),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({store, Start, Len}, State) ->
    store_item(Start, Len),
    lager:debug("stored, n=~p, len=~p", [Start, Len]),
    {noreply, State};
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

store_item(Start, Len) ->
    ets:insert(?TAB, {Start, Len}).

