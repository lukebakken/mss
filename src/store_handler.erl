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

handle_request(<<"DELETE">>, _, Req) ->
    handle_parse_request(parse_request(delete, Req));
handle_request(<<"GET">>, _, Req) ->
    handle_parse_request(parse_request(get, Req));
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
handle_parse_request({ok, Code, Body, ContentType, Req})
  when Code =:= 200, is_binary(Body), is_binary(ContentType) ->
    Req2 = cowboy_req:set_resp_header(<<"content-type">>, ContentType, Req),
    Req3 = cowboy_req:set_resp_body(Body, Req2),
    cowboy_req:reply(Code, Req3);
handle_parse_request({ok, Code, Req}) ->
    cowboy_req:reply(Code, Req).

parse_request(Type, Req) ->
    parse_binding(cowboy_req:binding(location, Req), Type).

parse_binding({undefined, Req}, _Type) ->
    {error, 400, <<"error|<location> not provided">>, Req};
parse_binding({Location, Req}, delete) when is_binary(Location) ->
    handle_delete(blob_mgr:delete(Location), Req);
parse_binding({Location, Req}, get) when is_binary(Location) ->
    handle_fetch(blob_mgr:fetch(Location), Req);
parse_binding({Location, Req}, Type) when is_binary(Location) ->
    % TODO FIXME infinite limit would not be used in production
    {ok, Body, Req2} = cowboy_req:body(Req, [{length, infinity}]),
    {ContentType, Req3} = cowboy_req:header(<<"content-type">>, Req2),
    Blob = #blob{id=Location, content_type=ContentType},
    handle_store(blob_mgr:store(Blob, Body, Type), Req3).

handle_delete({error, notfound}, Req) ->
    {error, 404, "", Req};
handle_delete({error, Msg}, Req) ->
    {error, 400, Msg, Req};
handle_delete(ok, Req) ->
    {ok, 204, Req}.

handle_fetch({error, notfound}, Req) ->
    {error, 404, "", Req};
handle_fetch({error, Msg}, Req) ->
    {error, 400, Msg, Req};
handle_fetch({ok, #blob{content_type=ContentType}, Body}, Req) ->
    {ok, 200, Body, ContentType, Req}.

handle_store({error, Msg}, Req) ->
    {error, 400, Msg, Req};
handle_store({ok, updated}, Req) ->
    {ok, 204, Req};
handle_store({ok, created}, Req) ->
    {ok, 201, Req}.
