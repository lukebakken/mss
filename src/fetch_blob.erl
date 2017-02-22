-module(fetch_blob).

-export([fetch/1]).

fetch(Id) ->
    FileName = util:get_filename(Id),
    handle_read_file(file:read_file(FileName), Id).

handle_read_file({error, enoent}, _Id) ->
    {error, notfound};
handle_read_file({error, Reason}, Id) ->
    Msg = io_lib:format("error|error reading blob with ID '~s': ~p", [Id, Reason]),
    {error, Msg};
handle_read_file({ok, FileBlob}, _Id) ->
    {ok, binary_to_term(FileBlob)}.
