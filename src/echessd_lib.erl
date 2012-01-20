-module(echessd_lib).

-export([ip2str/1]).

ip2str({A, B, C, D}) ->
    io_lib:format("~B.~B.~B.~B", [A, B, C, D]).

