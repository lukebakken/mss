-module(blob).

-include("mss.hrl").
-include("blob.hrl").

-export([store/3, fetch/1, delete/1]).

store(#blob{id=Id}=Blob, Bin, Type) ->
    Request = {store, Blob, Bin, Type},
    handle_get_mgr(blob_mgr_sup:get_mgr(Id), Request).

fetch(Id) ->
    Request = {fetch, Id},
    handle_get_mgr(blob_mgr_sup:get_mgr(Id), Request).

delete(Id) ->
    Request = {delete, Id},
    handle_get_mgr(blob_mgr_sup:get_mgr(Id), Request).

handle_get_mgr({error, Reason}, _) ->
    lager:error("blob handle_get_mgr error: ~p", [Reason]),
    {error, Reason};
handle_get_mgr({ok, Pid}, Request) when is_pid(Pid) ->
    gen_server:call(Pid, Request, ?DEFAULT_TIMEOUT).
