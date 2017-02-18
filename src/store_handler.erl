-module(store_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init({tcp, http}, Req, _Opts) ->
	{ok, Req, no_state}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req2),
    {ok, ReqRsp} = handle_store(Method, HasBody, Req2),
    {ok, ReqRsp, State}.

terminate(_Reason, _Req, _State) ->
	ok.

handle_store(<<"POST">>, true, Req) ->
    {ok, _PostVals, Req2} = cowboy_req:body_qs(Req),
    cowboy_req:reply(204, Req2);
handle_store(<<"POST">>, false, Req) ->
    cowboy_req:reply(400, [], <<"[error]: Missing body.">>, Req);
handle_store(_, _, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).
