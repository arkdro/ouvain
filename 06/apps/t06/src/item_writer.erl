-module(item_writer).

-behaviour(gen_server).

%% API
-export([
         store/4,
         start_link/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          fd
         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

store(Gtin, Name, Desc, Company) ->
    gen_server:cast(?SERVER, {store, Gtin, Name, Desc, Company}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    self() ! init,
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({store, Gtin, Name, Desc, Company}, State) ->
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(init, #state{fd = undefined} = State) ->
    init(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

init() ->
    ok.

