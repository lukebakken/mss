-module(util).

-include("mss.hrl").

-export([get_location/1, get_filename/1, get_bin_filename/1]).

get_location(Segments) when is_list(Segments) ->
    Joined = join(Segments, $/),
    erlang:iolist_to_binary(Joined).

get_filename(Id) ->
    {ok, BlobDir} = application:get_env(?APP_NAME, blob_dir),
    filename:join(BlobDir, base64:encode(Id)).

get_bin_filename(Id) ->
    {ok, BlobDir} = application:get_env(?APP_NAME, blob_dir),
    Name = io_lib:format("~s.bin", [base64:encode(Id)]),
    filename:join(BlobDir, Name).

%% @doc Join a a list of elements adding a separator between
%%      each of them.
%%      https://gist.github.com/jcomellas/5084441
-spec join(iolist(), Sep :: term()) -> iolist().
join([Head | Tail], Sep) ->
    join_list_sep(Tail, Sep, [Head]);
join([], _Sep) ->
    [].

-spec join_list_sep(iolist(), Sep :: term(), Acc :: iolist()) -> iolist().
join_list_sep([Head | Tail], Sep, Acc) ->
    join_list_sep(Tail, Sep, [Head, Sep | Acc]);
join_list_sep([], _Sep, Acc) ->
    lists:reverse(Acc).
