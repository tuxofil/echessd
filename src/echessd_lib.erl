-module(echessd_lib).

-export([ip2str/1, read_file/1,
         proplist_replace/2,
         timestamp/1
        ]).

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

proplist_replace(PropList, NewValues) ->
    lists:foldl(
      fun({Key, _V} = Item, Acc) ->
              [Item |
               [I || {K, _} = I <- Acc,
                     K /= Key]]
      end, PropList, NewValues).

timestamp(Time) ->
    {{Year, Month, Day}, {Hour, Minutes, Seconds}} =
        calendar:now_to_local_time(Time),
    io_lib:format(
      "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B",
      [Year, Month, Day, Hour, Minutes, Seconds]).

