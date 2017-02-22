-module(store_blob).

-include("blob.hrl").
-include("mss.hrl").

-export([store/2]).

store(#blob{id=Id}=Blob, Type) ->
    handle_get_filename(util:get_filename(Id), Blob, Type).

handle_get_filename(FileName, #blob{id=Id}=Blob, post) ->
    % post request allows for overwrite
    Bin = term_to_binary(Blob),
    store_blob(Bin, Id, FileName);
handle_get_filename(FileName, #blob{id=Id}=Blob, put) ->
    % put request enforces idempotency
    Bin = term_to_binary(Blob),
    handle_same_blob(is_same_blob(Bin, FileName), Bin, Id, FileName).

handle_same_blob({error, Reason}, _Bin, Id, _FileName) ->
    Msg = io_lib:format("error|id '~s': ~p", [Id, Reason]),
    {error, Msg};
handle_same_blob(true, _Bin, Id, FileName) ->
    Msg = io_lib:format("error|blob with id '~s' already exists (~s)", [Id, FileName]),
    {error, Msg};
handle_same_blob(false, Bin, Id, FileName) ->
    store_blob(Bin, Id, FileName).

store_blob(Bin, Id, FileName) ->
    handle_write_file(file:write_file(FileName, Bin), Id).

handle_write_file({error, Reason}, Id) ->
    Msg = io_lib:format("error|could not write blob with id '~s': ~p", [Id, Reason]),
    {error, Msg};
handle_write_file(ok, _Blob) ->
    {ok, created}.

-spec is_same_blob(binary(), string()) -> boolean()|{error, any()}.
is_same_blob(Bin, FileName) ->
    handle_is_regular_file(filelib:is_regular(FileName), Bin, FileName).

handle_is_regular_file(false, _Bin, _FileName) ->
    false;
handle_is_regular_file(true, Bin, FileName) ->
    handle_read_file(file:read_file(FileName), Bin).

handle_read_file({error, Reason}, _Bin) ->
    Msg = io_lib:format("error|~p", [Reason]),
    {error, Msg};
handle_read_file({ok, FileBlob}, Bin) ->
    % NB returns true or false
    erlang:md5(FileBlob) =:= erlang:md5(Bin).
