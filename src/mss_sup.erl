-module(mss_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildSpecs = [
        {blob_mgr_sup, {blob_mgr_sup, start_link, []}, permanent, 5, worker, [blob_mgr_sup]},
        {blob_proc_mgr, {blob_proc_mgr, start_link, []}, permanent, 5, worker, [blob_proc_mgr]}
    ],
    SupFlags = {one_for_one, 1, 5},
    {ok, {SupFlags, ChildSpecs}}.
