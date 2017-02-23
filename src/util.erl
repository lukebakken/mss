-module(util).

-include("mss.hrl").

-export([get_filename/1, get_bin_filename/1]).

get_filename(Id) ->
    {ok, BlobDir} = application:get_env(?APP_NAME, blob_dir),
    filename:join(BlobDir, base64:encode(Id)).

get_bin_filename(Id) ->
    {ok, BlobDir} = application:get_env(?APP_NAME, blob_dir),
    Name = io_lib:format("~s.bin", [base64:encode(Id)]),
    filename:join(BlobDir, Name).
