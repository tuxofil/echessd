%%% @doc
%%% Common utilities and library functions.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 20 Jan 2012
%%% @copyright 2012, Aleksey Morarash

-module(echessd_lib).

-export(
   [ip2str/1,
    timestamp/1,
    timestamp/2,
    random_elem/1,
    unconsult/3,
    escape_html_entities/1,
    administrative_offsets/0,
    time_offset_to_list/1,
    list_to_time_offset/1,
    local_offset/0,
    strip/2,
    string_to_terms/1,
    list_to_atom/2,
    list_to_atom/3,
    split4pathNquery/1
   ]).

%% ----------------------------------------------------------------------
%% Type definitions
%% ----------------------------------------------------------------------

-export_type([administrative_offset/0]).

-type administrative_offset() ::
        {Sign    :: -1 | 1,
         Hours   :: 0..14,
         Minutes :: 0 | 30 | 45}.

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Convert IPv4 address to canonic text representation.
-spec ip2str(inet:ip4_address()) -> nonempty_string().
ip2str({A, B, C, D}) ->
    io_lib:format("~B.~B.~B.~B", [A, B, C, D]).

%% @doc Format time as text.
-spec timestamp(erlang:timestamp()) -> nonempty_string().
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

%% @doc Format time as text with the specified time offset.
-spec timestamp(erlang:timestamp(), administrative_offset()) ->
                       nonempty_string().
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

%% @doc Return random element of the given list.
-spec random_elem(list()) -> any().
random_elem([Item]) ->
    Item;
random_elem([_ | _] = List) ->
    lists:nth(random:uniform(length(List)), List).

%% @doc Write the Erlang terms to a file as text.
%% The file then can be read by file:consult/1,
%% resulting the same Erlang terms.
-spec unconsult(Filename :: file:filename(),
                Comment :: string(),
                Terms :: list()) ->
                       ok | {error, Reason :: any()}.
unconsult(Filename, Comment, Terms) ->
    CommentBlock =
        case Comment of
            [_ | _] ->
                ["%% ",
                 re:replace(
                   strip(lists:flatten(Comment), "\n\r \t"),
                   "\n", "\n%% ", [global, {return, list}]),
                 "\n\n"];
            _ -> ""
        end,
    file:write_file(
      Filename,
      [CommentBlock,
       [io_lib:format("~80p.~n~n", [T]) || T <- Terms]]).

%% @doc Convert all chars with special meaning in HTML to
%%      HTML entities.
-spec escape_html_entities(String :: string()) -> NewString :: string().
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

%% @doc Return a list of all adminitrative time offsets available.
-spec administrative_offsets() -> [administrative_offset()].
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

%% @doc Format the time offset as for ISO8601 timestamp.
-spec time_offset_to_list(Offset :: administrative_offset()) ->
                                 nonempty_string().
time_offset_to_list({S, H, M}) ->
    Sign =
        if S == -1 -> "-";
           true -> "+"
        end,
    lists:flatten(
      io_lib:format("~s~2..0B~2..0B", [Sign, H, M])).

%% @doc Parse the time offset string representation.
-spec list_to_time_offset(String :: string()) ->
                                 {ok, Offset :: administrative_offset()} |
                                 error.
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

%% @doc Return the local time offset.
-spec local_offset() -> administrative_offset().
local_offset() ->
    local_offset(now()).

%% @doc Remove the Characters from the beginning and
%% from the end of the String.
-spec strip(String :: string(), Characters :: string()) ->
                   StrippedString :: string().
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

%% @doc Parse the input string to a list of Erlang terms.
-spec string_to_terms(String :: string()) -> [Term :: any()].
string_to_terms(String) ->
    {ok, Tokens, _EndLocation} = erl_scan:string(String),
    [begin
         {ok, Term} = erl_parse:parse_term(TermTokens),
         Term
     end || TermTokens <- explode_tokens(Tokens)].

%% @doc "Convert" string to an atom only if the result atom
%% is present in the list, supplied as the second argument.
-spec list_to_atom(String :: string(), Atoms :: [atom()]) ->
                          {ok, Atom :: atom()} | error.
list_to_atom(_, []) ->
    error;
list_to_atom(String, [Atom | Tail]) -> 
    case atom_to_list(Atom) of
        String ->
            {ok, Atom};
        _ ->
            list_to_atom(String, Tail)
    end.

%% @doc Same as list_to_atom/2 but return the given default value
%% on convertion error.
-spec list_to_atom(String :: string(), Atoms :: [atom()],
                   DefaultValue :: atom()) -> Atom :: atom().
list_to_atom(String, Atoms, DefaultValue) ->
    case list_to_atom(String, Atoms) of
        {ok, Value} ->
            Value;
        error ->
            DefaultValue
    end.

%% @doc Split the request URI to a Path and a Query strings
%% (delimited by '?').
-spec split4pathNquery(RequestURI :: string()) ->
                              {Path :: string(), Query :: string()}.
split4pathNquery(RequestURI) ->
    split4pathNquery(RequestURI, []).

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc Return the local time offset.
-spec local_offset(Time :: erlang:timestamp()) ->
                          Offset :: administrative_offset().
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

%% @doc
-spec explode_tokens(Tokens :: erl_scan:tokens()) ->
                            [TermTokens :: erl_scan:tokens()].
explode_tokens(Tokens) ->
    explode_tokens(Tokens, _Acc = []).

%% @doc
-spec explode_tokens(Tokens :: erl_scan:tokens(),
                     Acc :: [TermTokens :: erl_scan:tokens()]) ->
                            [TermTokens :: erl_scan:tokens()].
explode_tokens([], Acc) ->
    lists:reverse(Acc);
explode_tokens(Tokens, Acc) ->
    {TermTokens, Tail} = split_tokens(Tokens),
    explode_tokens(Tail, [TermTokens | Acc]).

%% @doc
-spec split_tokens(Tokens :: erl_scan:tokens()) ->
                          {TermTokens :: erl_scan:tokens(),
                           RestOfTokens :: erl_scan:tokens()}.
split_tokens(Tokens) ->
    split_tokens(Tokens, _Acc = []).

%% @doc
-spec split_tokens(Tokens :: erl_scan:tokens(),
                   Acc :: erl_scan:tokens()) ->
                          {TermTokens :: erl_scan:tokens(),
                           RestOfTokens :: erl_scan:tokens()}.
split_tokens([{dot, _} = Dot | Tail], Acc) ->
    {lists:reverse([Dot | Acc]), Tail};
split_tokens([Token | Tail], Acc) ->
    split_tokens(Tail, [Token | Acc]).

%% @doc
-spec split4pathNquery(RequestURI :: string(), Acc :: string()) ->
                              {Path :: string(), Query :: string()}.
split4pathNquery([$? | Tail], Path) ->
    {lists:reverse(Path), Tail};
split4pathNquery([H | Tail], Path) ->
    split4pathNquery(Tail, [H | Path]);
split4pathNquery(_, Path) ->
    {lists:reverse(Path), []}.

