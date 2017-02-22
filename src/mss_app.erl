-module(mss_app).
-behaviour(application).

-include("mss.hrl").

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    lager:start(),
    % Ensure priv_dir exists
    Dir = code:priv_dir(?APP_NAME),
    handle_make_dir(file:make_dir(Dir), Dir).

handle_make_dir({error, Reason}, PrivDir) ->
    Msg = io_lib:format("could not create blob dir '~p': ~p", [PrivDir, Reason]),
    {error, Msg};
handle_make_dir(ok, _PrivDir) ->
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
