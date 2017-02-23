-module(fetch_blob).

-export([fetch/1]).

fetch(Id) ->
    FileName = util:get_filename(Id),
    BinFileName = util:get_bin_filename(Id),
    FileRslt = file:read_file(FileName),
    BinFileRslt = file:read_file(BinFileName),
    handle_read_file({FileRslt, BinFileRslt}, Id).

handle_read_file({{error, enoent}, _}, _Id) ->
    {error, notfound};
handle_read_file({_, {error, enoent}}, _Id) ->
    {error, notfound};
handle_read_file({{error, Reason}, _}, Id) ->
    Msg = io_lib:format("error|error reading blob with ID '~s': ~p", [Id, Reason]),
    {error, Msg};
handle_read_file({_, {error, Reason}}, Id) ->
    Msg = io_lib:format("error|error reading blob data with ID '~s': ~p", [Id, Reason]),
    {error, Msg};
handle_read_file({{ok, FileBlob}, {ok, Data}}, _Id) ->
    {ok, binary_to_term(FileBlob), Data}.
