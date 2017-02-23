-module(store_blob).

-include("blob.hrl").
-include("mss.hrl").

-export([store/3]).

store(#blob{id=Id}=Blob, Bin, Type) ->
    handle_get_filename(util:get_filename(Id), Blob, Bin, Type).

handle_get_filename(FileName, #blob{id=Id}=Blob, Bin, post) ->
    % post request allows create only
    BlobTtb = term_to_binary(Blob),
    handle_blob_exists(blob_exists(FileName), post, BlobTtb, Bin, Id, FileName);
handle_get_filename(FileName, #blob{id=Id}=Blob, Bin, put) ->
    % put request allows update only
    BlobTtb = term_to_binary(Blob),
    handle_blob_exists(blob_exists(FileName), put, BlobTtb, Bin, Id, FileName).

handle_blob_exists({error, Reason}, _Type, _BlobTtb, _Bin, Id, _FileName) ->
    Msg = io_lib:format("error|ID '~s': ~p", [Id, Reason]),
    {error, Msg};
handle_blob_exists(false, post, BlobTtb, Bin, Id, FileName) ->
    store_blob(post, BlobTtb, Bin, Id, FileName);
handle_blob_exists(true, post, _BlobTtb, _Bin, Id, _FileName) ->
    Msg = io_lib:format("error|blob with ID '~s' exists, use PUT to update.", [Id]),
    {error, Msg};
handle_blob_exists(false, put, _BlobTtb, _Bin, Id, _FileName) ->
    Msg = io_lib:format("error|blob with ID '~s' does not yet exist, use POST to create.", [Id]),
    {error, Msg};
handle_blob_exists(true, put, BlobTtb, Bin, Id, FileName) ->
    store_blob(put, BlobTtb, Bin, Id, FileName).

store_blob(Type, BlobTtb, Bin, Id, FileName) ->
    BinFileName = util:get_bin_filename(Id),
    handle_write_bin_file(file:write_file(BinFileName, Bin), BlobTtb, Type, Id, FileName).
    % handle_write_file(file:write_file(FileName, Bin), Type, Id, FileName).

handle_write_bin_file({error, Reason}, _BlobTtb, _Type, Id, FileName) ->
    Msg = io_lib:format("error|could not write blob data with ID '~s' to file '~s': ~p", [Id, FileName, Reason]),
    {error, Msg};
handle_write_bin_file(ok, BlobTtb, Type, Id, FileName) ->
    handle_write_file(file:write_file(FileName, BlobTtb), Type, Id, FileName).

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
