-module(mss_app).
-behaviour(application).
-include("mss.hrl").

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    lager:start(),
    Dispatch = cowboy_router:compile([
        {'_', [
               {"/store/[:location]", store_handler, []},
               {"/", generic_handler, []}
        ]}
    ]),
    TOpts = [{port, 7777}, {ip, {0,0,0,0}}],
    POpts = [{env, [{dispatch, Dispatch}]}],
    {ok, _Pid} = cowboy:start_http(mss_http_listener, 100, TOpts, POpts),
	mss_sup:start_link().

stop(_State) ->
	ok.
