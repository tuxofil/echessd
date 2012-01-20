-module(echessd_lib).

-export([ip2str/1, read_file/1]).

ip2str({A, B, C, D}) ->
    io_lib:format("~B.~B.~B.~B", [A, B, C, D]).

read_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            binary_to_list(Binary);
        {error, Reason} ->
            echessd_log:err(
              "Unable to read ~9999p: ~9999p",
              [Filename, Reason]),
            throw(Reason)
    end.

