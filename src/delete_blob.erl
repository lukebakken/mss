-module(delete_blob).

-export([delete/1]).

delete(Id) ->
    FileName = util:get_filename(Id),
    handle_delete_file(file:delete(FileName), Id).

handle_delete_file({error, enoent}, _Id) ->
    {error, notfound};
handle_delete_file({error, Reason}, Id) ->
    Msg = io_lib:format("error|error deleting blob with ID '~s': ~p", [Id, Reason]),
    {error, Msg};
handle_delete_file(ok, _Id) ->
    ok.
