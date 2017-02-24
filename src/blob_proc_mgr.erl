-module(blob_proc_mgr).
-behaviour(gen_server).

-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/0, stop/0,
         get_pid/1, save_pid/2]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    ok = lager:debug("blob_proc_mgr start_link"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

get_pid(Id) ->
    gen_server:call(?MODULE, {get_pid, Id}).

save_pid(Id, Pid) ->
    gen_server:call(?MODULE, {save_pid, Id, Pid}).

init([]) ->
    ok = lager:debug("blob_proc_mgr init"),
    {ok, no_state}.

handle_call({get_pid, Id}, _From, State) ->
    Result = get(Id),
    {reply, Result, State};
handle_call({save_pid, Id, Pid}, _From, State) ->
    undefined = put(Id, Pid),
    ok = lager:debug("blob_proc_mgr save_pid proc dict: ~p", [get()]),
    {reply, ok, State};
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
