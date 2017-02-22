-module(util).

-include("mss.hrl").

-export([get_filename/1]).

get_filename(Id) ->
    Dir = code:priv_dir(?APP_NAME),
    filename:join(Dir, base64:encode(Id)).
