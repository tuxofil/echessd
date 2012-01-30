%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 20 Jan 2012
%%% @copyright 2012, Aleksey Morarash
%%% @doc Common utilities and library functions.

-module(echessd_lib).

-export([ip2str/1,
         proplist_replace/2,
         timestamp/1,
         timestamp/2,
         random_elem/1,
         escape_html_entities/1,
         gettext/2,
         languages/0,
         parse_language/1,
         administrative_offsets/0,
         time_offset_to_list/1,
         list_to_time_offset/1,
         local_offset/0,
         strip/2
        ]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% Type definitions
%% ----------------------------------------------------------------------

-export_type([administrative_offset/0]).

-type administrative_offset() ::
        {Sign    :: -1 | 1,
         Hours   :: 0..14,
         Minutes :: 0 | 30 | 45
        }.

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
%% @spec timestamp(timestamp()) -> string()
timestamp(Time) ->
    {{Year, Month, Day}, {Hour, Minutes, Seconds}} =
        calendar:now_to_local_time(Time),
    {OffSign, OffHours, OffMinutes} = local_offset(Time),
    Sign =
        if OffSign > 0 -> "+";
           true -> "-"
        end,
    io_lib:format(
      "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B~s~2..0B~2..0B",
      [Year, Month, Day, Hour, Minutes, Seconds,
       Sign, OffHours, OffMinutes]).

%% @doc Formats time as text with specified time offset.
%% @spec timestamp(timestamp(), administrative_offset()) -> string()
timestamp(Timestamp, {OffsetSign, OffsetHours, OffsetMinutes}) ->
    {{Year, Month, Day}, {Hour, Minutes, Seconds}} =
        calendar:gregorian_seconds_to_datetime(
          calendar:datetime_to_gregorian_seconds(
            calendar:now_to_universal_time(Timestamp)) +
              OffsetSign * (OffsetHours * 60 + OffsetMinutes) * 60),
    Sign =
        if OffsetSign > 0 -> "+";
           true -> "-"
        end,
    io_lib:format(
      "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B~s~2..0B~2..0B",
      [Year, Month, Day, Hour, Minutes, Seconds,
       Sign, OffsetHours, OffsetMinutes]).

%% @doc Return random element of the list supplied.
%% @spec random_elem(list()) -> term()
random_elem([Item]) -> Item;
random_elem(List) when is_list(List) ->
    lists:nth(random:uniform(length(List)), List).

%% @doc Converts all chars with special meaning in HTML to
%%      HTML entities.
%% @spec escape_html_entities(String) -> NewString
%%     String = string(),
%%     NewString = string()
escape_html_entities(String) ->
    lists:flatmap(
      fun($<) -> "&lt;";
         ($>) -> "&gt;";
         ($") -> "&quot;";
         ($') -> "&apos;";
         ($&) -> "&amp;";
         (C) ->
              [C]
      end, String).

%% @doc Fetches localized text with specified ID.
%% @spec gettext(TextID, LangID) -> string()
%%     TextID = term(),
%%     LangID = atom()
gettext(TextID, LangID) ->
    {_Languages, Strings} = echessd_cfg:get(?CFG_LANG_INFO),
    case dict:find({TextID, LangID}, Strings) of
        {ok, Text} -> Text;
        _ ->
            DefLang = echessd_cfg:get(?CFG_DEF_LANG),
            case dict:find({TextID, DefLang}, Strings) of
                {ok, Text} -> Text;
                _ ->
                    echessd_log:err(
                      "Failed to fetch text "
                      "~9999p for lang ~9999p "
                      "(default lang is ~9999p)",
                      [TextID, LangID, DefLang]),
                    ""
            end
    end.

%% @doc Return list of supported languages.
%% @spec languages() -> List
%%     List = [{LangAbbreviation, LangName}],
%%     LangAbbreviation = atom(),
%%     LangName = string()
languages() ->
    {Languages, _Strings} = echessd_cfg:get(?CFG_LANG_INFO),
    Languages.

%% @doc Parses language identifier.
%% @spec parse_language(String) -> {LangAbbreviation, LangName} | undefined
%%     LangAbbreviation = atom(),
%%     LangName = string()
parse_language([_ | _] = String) ->
    Languages = languages(),
    List = [{atom_to_list(N), N} || {N, _} <- Languages],
    Stripped = string:to_lower(strip(String, " \t\r\n")),
    case [V || {K, V} <- List, K == Stripped] of
        [Parsed | _] ->
            {Parsed, proplists:get_value(Parsed, Languages)};
        _ -> undefined
    end;
parse_language(_) -> undefined.

%% @doc Return list of all adminitrative time offsets.
%% @spec administrative_offsets() -> Offsets
%%     Offsets = [administrative_offset()]
administrative_offsets() ->
    Negative =
        [{12, 0}, {11, 0}, {10, 0}, {9, 30}, {9, 0},
         {8, 0}, {7, 0}, {6, 0}, {5, 0}, {4, 30},
         {4, 0}, {3, 30}, {3, 0}, {2, 0}, {1, 0}],
    Positive =
        [{0, 0}, {1, 0}, {2, 0}, {3, 0}, {3, 30},
         {4, 0}, {4, 30}, {5, 0}, {5, 30}, {5, 45},
         {6, 0}, {7, 0}, {8, 0}, {9, 0}, {9, 30},
         {10, 0}, {10, 30}, {11, 0}, {11, 30}, {12, 00},
         {12, 45}, {13, 00}, {14, 00}],
    [{-1, H, M} || {H, M} <- Negative] ++
        [{1, H, M} || {H, M} <- Positive].

%% @doc Formats time offset as for ISO8601 timestamp.
%% @spec time_offset_to_list(Offset) -> string()
%%     Offset = administrative_offset()
time_offset_to_list({S, H, M}) ->
    Sign =
        if S == -1 -> "-";
           true -> "+"
        end,
    lists:flatten(
      io_lib:format("~s~2..0B~2..0B", [Sign, H, M])).

%% @doc Parse time offset string representation.
%% @spec list_to_time_offset(String) -> {ok, Offset} | error
%%     String = string(),
%%     Offset = administrative_offset()
list_to_time_offset(String) ->
    case io_lib:fread("~c~2d~2d", String) of
        {ok, [Sign, H, M], []} ->
            S =
                if Sign == "+" -> 1;
                   true -> -1
                end,
            Tuple = {S, H, M},
            case lists:member(Tuple, administrative_offsets()) of
                true -> {ok, Tuple};
                _ -> error
            end;
        _ -> error
    end.

%% @doc Return local offset (on server).
%% @spec local_offset() -> Offset
%%     Offset = administrative_offset()
local_offset() -> local_offset(now()).

%% @doc Removes Characters from beginning and ending of String.
%% @spec strip(String, Characters) -> StrippedString
%%     String = string(),
%%     Characters = string(),
%%     StrippedString = string()
strip(String, Characters) ->
    lists:reverse(
      strip_(
        lists:reverse(
          strip_(String, Characters)),
        Characters)).
strip_([], _Chars) -> [];
strip_([Char | Tail] = String, Chars) ->
    case lists:member(Char, Chars) of
        true ->
            strip_(Tail, Chars);
        _ ->
            String
    end.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc Return local offset (on server).
%% @spec local_offset(Time) -> Offset
%%     Time = timestamp(),
%%     Offset = administrative_offset()
local_offset(Time) ->
    SecondsDiff =
        calendar:datetime_to_gregorian_seconds(
          calendar:now_to_local_time(Time)) -
        calendar:datetime_to_gregorian_seconds(
          calendar:now_to_universal_time(Time)),
    OffsetMinutes0 = round(abs(SecondsDiff) / (15 * 60)) * 15,
    OffsetHours = OffsetMinutes0 div 60,
    OffsetMinutes = OffsetMinutes0 rem 60,
    Sign =
        if SecondsDiff > 0 -> 1;
           true -> -1
        end,
    {Sign, OffsetHours, OffsetMinutes}.

