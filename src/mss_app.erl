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
    {ok, _Pid} = cowboy:start_http(my_http_listener, 100, [{port, 7777}],
      [
        {env, [{dispatch, Dispatch}]},
        {middlewares, [mss_debug, cowboy_router, cowboy_handler]}
      ]
    ),
	mss_sup:start_link().

stop(_State) ->
	ok.
