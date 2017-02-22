-module(store_handler).

-behaviour(cowboy_http_handler).

-include("blob.hrl").

-export([init/3,
         handle/2,
         terminate/3]).

init({tcp, http}, Req, _Opts) ->
	{ok, Req, no_state}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req2),
    {ok, ReqRsp} = handle_request(Method, HasBody, Req2),
    {ok, ReqRsp, State}.

terminate(_Reason, _Req, _State) ->
	ok.

handle_request(<<"POST">>, true, Req) ->
    handle_parse_request(parse_request(post, Req));
handle_request(<<"PUT">>, true, Req) ->
    handle_parse_request(parse_request(put, Req));
handle_request(<<"POST">>, false, Req) ->
    reply_error(400, <<"error|missing body">>, Req);
handle_request(<<"PUT">>, false, Req) ->
    reply_error(400, <<"error|missing body">>, Req);
handle_request(_, _, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).

reply_error(Code, Msg, Req) ->
    cowboy_req:reply(Code, [], Msg, Req).

handle_parse_request({error, Code, Msg, Req}) ->
    reply_error(Code, Msg, Req);
handle_parse_request({ok, Code, Req}) ->
    cowboy_req:reply(Code, Req).

parse_request(Type, Req) ->
    parse_binding(cowboy_req:binding(location, Req), Type).

parse_binding({undefined, Req}, _Type) ->
    {error, 400, <<"error|<location> not provided">>, Req};
parse_binding({Location, Req}, Type) when is_binary(Location) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    {ok, CT, Req3} = cowboy_req:parse_header(<<"content-type">>, Req2),
    B = #blob{id=Location, content_type=CT, body=Body},
    handle_store(filemgr:store(B, Type), Req3).

handle_store({error, Msg}, Req) ->
    {error, 400, Msg, Req};
handle_store({ok, created}, Req) ->
    {ok, 201, Req}.
