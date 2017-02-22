-module(util).

-include("mss.hrl").

-export([get_filename/1]).

get_filename(Id) ->
    {ok, BlobDir} = application:get_env(?APP_NAME, blob_dir),
    filename:join(BlobDir, base64:encode(Id)).
