-module(blob_mgr).
-behaviour(gen_server).

-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/0, stop/0,
         store/2, fetch/1, delete/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

store(Blob, Type) ->
    gen_server:call(?MODULE, {store, Blob, Type}).

fetch(Id) ->
    gen_server:call(?MODULE, {fetch, Id}).

delete(Id) ->
    gen_server:call(?MODULE, {delete, Id}).

init([]) ->
    State = no_state,
    ok = lager:debug("blob_mgr init: ~p", [State]),
    {ok, State}.

handle_call({store, Blob, Type}, _From, State) ->
    Result = store_blob:store(Blob, Type),
    {reply, Result, State};
handle_call({fetch, Id}, _From, State) ->
    Result = fetch_blob:fetch(Id),
    {reply, Result, State};
handle_call({delete, Id}, _From, State) ->
    Result = delete_blob:delete(Id),
    {reply, Result, State};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
