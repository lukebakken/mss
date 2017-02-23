-module(mss_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
        {blob_mgr, {blob_mgr, start_link, []}, permanent, 5, worker, [blob_mgr]}
    ],
	{ok, {{one_for_one, 1, 5}, Procs}}.
