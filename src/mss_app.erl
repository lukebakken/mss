-module(mss_app).
-behaviour(application).

-include("mss.hrl").

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    lager:start(),
    ensure_mss_blob_dir().

ensure_mss_blob_dir() ->
    handle_mss_getenv(os:getenv("MSS_BLOB_DIR")).

handle_mss_getenv(false) ->
    BlobDir = code:priv_dir(?APP_NAME),
    handle_make_dir(file:make_dir(BlobDir), BlobDir);
handle_mss_getenv(BlobDir) ->
    handle_make_dir(file:make_dir(BlobDir), BlobDir).

handle_make_dir({error, eexist}, BlobDir) ->
    start_mss_app(BlobDir);
handle_make_dir({error, Reason}, BlobDir) ->
    Msg = io_lib:format("could not create blob dir '~p': ~p", [BlobDir, Reason]),
    {error, Msg};
handle_make_dir(ok, BlobDir) ->
    start_mss_app(BlobDir).

start_mss_app(BlobDir) ->
    application:set_env(?APP_NAME, blob_dir, BlobDir),
    lager:debug("Using blob directory '~s'", [BlobDir]),
    start_cowboy().

start_cowboy() ->
    Dispatch = cowboy_router:compile([
        {'_', [
               {"/store/:location/[...]", store_handler, []},
               {"/", generic_handler, []}
        ]}
    ]),
    TOpts = [{port, 7777}, {ip, {0,0,0,0}}],
    POpts = [{env, [{dispatch, Dispatch}]}],
    {ok, _Pid} = cowboy:start_http(mss_http_listener, 100, TOpts, POpts),
    mss_sup:start_link().

stop(_State) ->
    ok.
