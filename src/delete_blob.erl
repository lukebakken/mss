-module(delete_blob).

-export([delete/1]).

delete(Id) ->
    FileName = util:get_filename(Id),
    BinFileName = util:get_bin_filename(Id),
    R1 = file:delete(FileName),
    R2 = file:delete(BinFileName),
    handle_delete_files({R1, R2}, Id).

handle_delete_files({{error, enoent}, _}, _Id) ->
    {error, notfound};
handle_delete_files({_, {error, enoent}}, _Id) ->
    {error, notfound};
handle_delete_files({{error, Reason}, _}, Id) ->
    return_error(Id, Reason);
handle_delete_files({_, {error, Reason}}, Id) ->
    return_error(Id, Reason);
handle_delete_files({ok, ok}, _Id) ->
    ok.

return_error(Id, Reason) ->
    Msg = io_lib:format("error|error deleting blob with ID '~s': ~p", [Id, Reason]),
    {error, Msg}.
