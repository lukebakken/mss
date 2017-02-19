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
    {ok, ReqRsp} = handle_request(Method, HasBody, Req2),
    {ok, ReqRsp, State}.

terminate(_Reason, _Req, _State) ->
	ok.

% TODO POST vs PUT
handle_request(<<"POST">>, true, Req) ->
    handle_parse_request(parse_request(Req));
handle_request(<<"PUT">>, true, Req) ->
    handle_parse_request(parse_request(Req));
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

parse_request(Req) ->
    parse_binding(cowboy_req:binding(location, Req)).

parse_binding({undefined, Req}) ->
    {error, 400, <<"error|<location> not provided">>, Req};
parse_binding({Location, Req}) when is_binary(Location) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    {ok, CT, Req3} = cowboy_req:parse_header(<<"content-type">>, Req2),
    % TODO use opaque record
    Object = [{body, Body}, {content_type, CT}],
    handle_filemgr_store(filemgr:store(Object), Req3).

handle_filemgr_store({error, Msg}, Req) ->
    {error, 400, Msg, Req};
handle_filemgr_store({ok, created}, Req) ->
    {ok, 201, Req}.
