-module(filemgr).

-include("blob.hrl").
-include("mss.hrl").

-export([store/1]).

store(#blob{id=Id}=Blob) ->
    handle_get_filename(get_filename(Id), Blob).

handle_get_filename({error, Msg}, _Blob) ->
    {error, Msg};
handle_get_filename(FileName, Blob) ->
    handle_file_exists(file_exists(FileName), Blob, FileName).

handle_file_exists(true, #blob{id=Id}, FileName) ->
    Msg = io_lib:format("blob with id '~p' already exists (~p)", [Id, FileName]),
    {error, Msg};
handle_file_exists(false, Blob, FileName) ->
    store_blob(Blob, FileName).

store_blob(Blob, FileName) ->
    Bin = term_to_binary(Blob),
    handle_write_file(file:write_file(FileName, Bin), Blob).

handle_write_file({error, Reason}, #blob{id=Id}) ->
    Msg = io_lib:format("could not write blob with id '~p': ~p", [Id, Reason]),
    {error, Msg};
handle_write_file(ok, _Blob) ->
    {ok, created}.

file_exists(FileName) ->
    filelib:is_regular(FileName).

get_filename(Id) ->
    Dir = code:priv_dir(?APP_NAME),
    filename:join(Dir, base64:encode(Id)).
