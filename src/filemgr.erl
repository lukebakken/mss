-module(filemgr).

-include("blob.hrl").
-include("mss.hrl").

-export([store/1]).

store(#blob{id=Id}) ->
    FN = get_filename(Id),
    handle_file_exists(file_exists(FN), FN).

handle_file_exists(true, FileName) ->
    Msg = iolib:format("file with name ~s already exists", [FileName]),
    {error, Msg};
handle_file_exists(false, _FileName) ->
    {ok, created}.

file_exists(FileName) ->
    filelib:is_regular(FileName).

get_filename(Id) ->
    Dir = code:priv_dir(?APP_NAME),
    filename:join(Dir, Id).
