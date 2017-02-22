-module(store_blob).

-include("blob.hrl").
-include("mss.hrl").

-export([store/2]).

store(#blob{id=Id}=Blob, Type) ->
    handle_get_filename(util:get_filename(Id), Blob, Type).

handle_get_filename(FileName, #blob{id=Id}=Blob, post) ->
    % post request allows create only
    Bin = term_to_binary(Blob),
    handle_blob_exists(blob_exists(FileName), post, Bin, Id, FileName);
handle_get_filename(FileName, #blob{id=Id}=Blob, put) ->
    % put request allows update only
    Bin = term_to_binary(Blob),
    handle_blob_exists(blob_exists(FileName), put, Bin, Id, FileName).

handle_blob_exists({error, Reason}, _Type, _Bin, Id, _FileName) ->
    Msg = io_lib:format("error|ID '~s': ~p", [Id, Reason]),
    {error, Msg};
handle_blob_exists(false, post, Bin, Id, FileName) ->
    store_blob(post, Bin, Id, FileName);
handle_blob_exists(true, post, _Bin, Id, _FileName) ->
    Msg = io_lib:format("error|blob with ID '~s' exists, use PUT to update.", [Id]),
    {error, Msg};
handle_blob_exists(false, put, _Bin, Id, _FileName) ->
    Msg = io_lib:format("error|blob with ID '~s' does not yet exist, use POST to create.", [Id]),
    {error, Msg};
handle_blob_exists(true, put, Bin, Id, FileName) ->
    store_blob(put, Bin, Id, FileName).

store_blob(Type, Bin, Id, FileName) ->
    handle_write_file(file:write_file(FileName, Bin), Type, Id, FileName).

handle_write_file({error, Reason}, _Type, Id, FileName) ->
    Msg = io_lib:format("error|could not write blob with ID '~s' to file '~s': ~p", [Id, FileName, Reason]),
    {error, Msg};
handle_write_file(ok, post, _Id, _FileName) ->
    {ok, created};
handle_write_file(ok, put, _Id, _FileName) ->
    {ok, updated}.

-spec blob_exists(string()) -> boolean().
blob_exists(FileName) ->
    filelib:is_regular(FileName).
