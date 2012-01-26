%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 20 Jan 2012
%%% @copyright 2012, Aleksey Morarash
%%% @doc Common utilities and library functions.

-module(echessd_lib).

-export([ip2str/1,
         proplist_replace/2,
         timestamp/1,
         random_elem/1
        ]).

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Converts IPv4 address to text representation.
%% @spec ip2str(ip_address()) -> string()
ip2str({A, B, C, D}) ->
    io_lib:format("~B.~B.~B.~B", [A, B, C, D]).

%% @doc Replaces items in proplist.
%% @spec proplist_replace(PropList, NewValues) -> NewPropList
%%     PropList = proplist(),
%%     NewValues = proplist(),
%%     NewPropList = proplist()
proplist_replace(PropList, NewValues) ->
    lists:foldl(
      fun({Key, _V} = Item, Acc) ->
              [Item |
               [I || {K, _} = I <- Acc,
                     K /= Key]]
      end, PropList, NewValues).

%% @doc Formats time as text.
%% @spec timestamp(now()) -> string()
timestamp(Time) ->
    {{Year, Month, Day}, {Hour, Minutes, Seconds}} =
        calendar:now_to_local_time(Time),
    io_lib:format(
      "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B",
      [Year, Month, Day, Hour, Minutes, Seconds]).

%% @doc Return random element of the list supplied.
%% @spec random_elem(list()) -> term()
random_elem([Item]) -> Item;
random_elem(List) when is_list(List) ->
    lists:nth(random:uniform(length(List)), List).

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

