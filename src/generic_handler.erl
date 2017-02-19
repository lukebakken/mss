-module(generic_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).
-export([generic_response/1]).

-record(state, {
}).

init({tcp, http}, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    {ok, ReqRsp} = generic_response(Req),
    {ok, ReqRsp, State}.

terminate(_Reason, _Req, _State) ->
	ok.

generic_response(Req) ->
    {ok, _ReqRsp} = cowboy_req:reply(200,
        [{<<"content-type">>, <<"text/plain">>}],
        <<"MSS">>,
        Req).

