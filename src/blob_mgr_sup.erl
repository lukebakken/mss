-module(blob_mgr_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

-export([get_mgr/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	ChildSpecs = [
        {blob_mgr, {blob_mgr, start_link, []}, transient, 5, worker, [blob_mgr]}
    ],
    SupFlags = {simple_one_for_one, 1, 5},
	{ok, {SupFlags, ChildSpecs}}.

get_mgr(Id) ->
    maybe_start_child(blob_proc_mgr:get_pid(Id), Id).

maybe_start_child(undefined, Id) ->
    handle_start_child(supervisor:start_child(?MODULE, [Id]), Id);
maybe_start_child(Pid, _Id) when is_pid(Pid) ->
    {ok, Pid}.

handle_start_child({error, Reason}, Id) ->
    Msg = io_lib:format("error|error starting child for id '~s': ~p", [Id, Reason]),
    {error, Msg};
handle_start_child({ok, Pid}, Id) ->
    handle_save_pid(blob_proc_mgr:save_pid(Id, Pid), Pid);
handle_start_child({ok, Pid, Info}, Id) ->
    ok = lager:debug("blob_mgr_sup handle_start_child info: ~p", [Info]),
    handle_save_pid(blob_proc_mgr:save_pid(Id, Pid), Pid).

handle_save_pid(ok, Pid) ->
    {ok, Pid};
handle_save_pid(Result, _Pid) ->
    ok = lager:error("blob_mgr_sup handle_save_pid result: ~p", [Result]),
    error.
