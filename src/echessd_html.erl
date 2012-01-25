%%%-------------------------------------------------------------------
%%% File    : echessd_html.erl
%%% Author  : Aleksey Morarash <aleksey.morarash@gmail.com>
%%% Created : 21 Jan 2012
%%% License : FreeBSD
%%% Description : HTML pages generation
%%%
%%%-------------------------------------------------------------------

-module(echessd_html).

-export([login/0,
         register/0,
         error/1,
         eaccess/0,
         home/0,
         game/1,
         history/1,
         users/0,
         user/1,
         newgame/0,
         notyet/0
        ]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Makes 'login' page content.
%% @spec login() -> io_list()
login() ->
    html_page_header("echessd - Login", [{h1, "echessd login"}]) ++
        navig_links([{"?goto=" ++ ?SECTION_REG, "Register new user"}]) ++
        "<form method=post>"
        "<input name=action type=hidden value=login>"
        "Login:    <input name=username type=text><br>"
        "Password: <input name=password type=password><br>"
        "<input type=submit value='Login'>"
        "</form>" ++
        html_page_footer([]).

%% @doc Makes 'register new user' page content.
%% @spec register() -> io_list()
register() ->
    html_page_header("echessd - Register new user",
                     [{h1, "echessd register form"}]) ++
        navig_links([{"?goto=" ++ ?SECTION_LOGIN, "Return to login form"}]) ++
        "<form method=post>"
        "<input name=action type=hidden value=register>"
        "Login:    <input name=regusername type=text><br>"
        "Full name: <input name=regfullname type=text> (optional)<br>"
        "Password: <input name=regpassword1 type=password><br>"
        "Confirm password: <input name=regpassword2 type=password><br>"
        "<input type=submit value='Register'>"
        "</form>" ++
        html_page_footer([]).

%% @doc Makes 'home' page content.
%% @spec home() -> io_list()
home() ->
    Username = get(username),
    {ok, UserProperties} = echessd_user:getprops(Username),
    html_page_header(
      "echessd - Home", [{h1, "Logged in as '" ++ Username ++ "'"}]) ++
        navigation() ++
        "<br>" ++
        user_info(Username, UserProperties) ++
        "<br>" ++
        user_games(Username, UserProperties) ++
        "<br>" ++
        newgame_link(Username) ++
        html_page_footer([]).

%% @doc Makes 'registered users list' page content.
%% @spec users() -> io_list()
users() ->
    {ok, Users0} = echessd_user:list(),
    Users = lists:usort(Users0) -- get(username),
    Title = "User list",
    html_page_header("echessd - " ++ Title, [{h1, Title}]) ++
        navigation() ++
        "<br>" ++
        string:join(
          lists:map(
            fun(Username) ->
                    "*&nbsp;" ++ userlink(Username)
            end, Users), "<br>") ++
        html_page_footer([]).

%% @doc Makes 'user details' page content.
%% @spec user(Username) -> io_list()
%%     Username = echessd_user:echessd_user()
user(Username) ->
    case get(username) of
        Username -> home();
        _ ->
            case echessd_user:getprops(Username) of
                {ok, UserProperties} ->
                    user(Username, UserProperties);
                {error, Reason} ->
                    formatted_error_page(
                      "Unable to fetch user ~9999p properties:<br>" ++ tt("~p"),
                      [Username, Reason])
            end
    end.
user(Username, UserProperties) ->
    Title = "User '" ++ Username ++ "'",
    html_page_header("echessd - " ++ Title, [{h1, Title}]) ++
        navigation() ++
        "<br>" ++
        user_info(Username, UserProperties) ++
        "<br>" ++
        user_games(Username, UserProperties) ++
        "<br>" ++
        newgame_link(Username) ++
        html_page_footer([]).

%% @doc Makes 'create new game' page content.
%% @spec newgame() -> io_list()
newgame() ->
    Opponent = proplists:get_value("user", get(query_proplist)),
    case echessd_user:getprops(Opponent) of
        {ok, OpponentProperties} ->
            newgame(Opponent, OpponentProperties);
        {error, Reason} ->
            formatted_error_page(
              "Unable to fetch user ~9999p properties:<br>" ++ tt("~p"),
              [Opponent, Reason])
    end.
newgame(Opponent, OpponentProperties) ->
    echessd_session:set_val(
      opponent, {Opponent, OpponentProperties}),
    Iam = get(username),
    H2Title =
        if Iam == Opponent ->
                "Test game: " ++ Iam ++ " vs " ++ Iam;
           true -> "Game: " ++ Iam ++ " vs " ++ Opponent
        end,
    ColorSelector =
        if Iam == Opponent ->
                "<input name=color type=hidden value=white>";
           true ->
                "Color: <select name=color>"
                    "<option value='random'>Choose randomly</option>"
                    "<option value='white'>White</option>"
                    "<option value='black'>Black</option>"
                    "</select><br>"
        end,
    html_page_header("echessd - New game", [{h1, "New game"}]) ++
        navigation() ++
        h2(H2Title) ++
        "<form method=post>"
        "<input name=action type=hidden value=" ++ ?SECTION_NEWGAME ++ ">"
        "<input name=gametype type=hidden value=classic>"
        ++ ColorSelector ++
        "<input type=submit value='Create'>"
        "</form>" ++
        html_page_footer([]).

%% @doc Makes 'game' page content.
%% @spec game(GameID) -> io_list()
%%     GameID = echessd_game:echessd_game_id()
game(GameID) ->
    Iam = get(username),
    case echessd_game:fetch(GameID) of
        {ok, GameInfo, {Board, Captures}} ->
            GameType = proplists:get_value(type, GameInfo),
            Players =
                [I || {users, L} <- GameInfo, {_, C} = I <- L,
                      lists:member(C, [?black, ?white])],
            Users = lists:usort([N || {N, _} <- Players]),
            MyColors = [C || {N, C} <- Players, N == Iam],
            TurnColor = echessd_game:turn_color(GameInfo),
            TurnUser = hd([N || {N, C} <- Players, C == TurnColor]),
            OppColor = hd([?black, ?white] -- [TurnColor]),
            OppUser = hd([N || {N, C} <- Players, C == OppColor]),
            IsRotated =
                if TurnUser == OppUser andalso
                   OppUser == Iam ->
                        TurnColor == ?black;
                   true ->
                        lists:member(Iam, Users) andalso
                            lists:member(?black, MyColors)
                end,
            html_page_header(
              "echessd - Game",
              [{h1, "Game #" ++ integer_to_list(GameID)}]) ++
                navigation() ++
                game_navigation(GameID) ++
                chess_table(GameType, Board, IsRotated) ++
                case TurnUser of
                    Iam ->
                        "<form method=post>"
                            "Move:&nbsp;"
                            "<input name=action type=hidden value=move>"
                            "<input name=game type=hidden value=" ++
                            integer_to_list(GameID) ++ ">"
                            "<input name=move type=text size=4>"
                            "<input type=submit value=OK>"
                            "<input type=reset value=Reset>"
                            "</form>";
                    _ -> ""
                end ++
                "<br>" ++
                captures(Captures) ++
                html_page_footer([]);
        {error, Reason} ->
            formatted_error_page(
              "Unable to fetch game #~9999p properties:<br>" ++ tt("~p"),
              [GameID, Reason])
    end.

%% @doc Makes 'game history' page content.
%% @spec history(GameID) -> io_list()
%%     GameID = echessd_game:echessd_game_id()
history(GameID) ->
    case echessd_game:getprops(GameID) of
        {ok, GameInfo} ->
            GameType = proplists:get_value(type, GameInfo),
            FullHistory = proplists:get_value(moves, GameInfo),
            FullHistoryLen = length(FullHistory),
            StrStep = proplists:get_value("step", get(query_proplist)),
            Step =
                try list_to_integer(StrStep) of
                    Int when Int >= 0 andalso Int =< FullHistoryLen ->
                        Int;
                    _ -> FullHistoryLen
                catch
                    _:_ -> FullHistoryLen
                end,
            History = lists:sublist(FullHistory, Step),
            {Board, Captures} =
                echessd_game:from_scratch(GameType, History),
            html_page_header(
              "echessd - Game history",
              [{h1, "Game #" ++ integer_to_list(GameID) ++ " history"}]) ++
                navigation() ++
                navig_links(
                  [{"?goto=" ++ ?SECTION_GAME ++
                        "&game=" ++ integer_to_list(GameID),
                    "Return to game"}]) ++
                history_navigation(GameID, Step, FullHistoryLen) ++
                chess_table(GameType, Board, false) ++
                captures(Captures) ++
                html_page_footer([]);
        {error, Reason} ->
            formatted_error_page(
              "Unable to fetch game #~9999p properties:<br>" ++ tt("~p"),
              [GameID, Reason])
    end.

%% @doc Makes 'under construction' page content.
%% @spec notyet() -> io_list()
notyet() ->
    html_page_header("echessd - Under construction",
                     [{h1, "Not implemented yet"}]) ++
        navig_links([{"javascript: history.back();", "Back"}]) ++
        html_page_footer([]).

%% @doc Makes 'error' page content.
%% @spec error(Message) -> io_list()
%%     Message = io_list()
error(Message) ->
    html_page_header("echessd - Error", [{h1, "echessd error"}]) ++
        tag("div", ["class=error"], Message) ++
        "<br>" ++
        navig_links([{"javascript: history.back();", "Back"}]) ++
        html_page_footer([]).

%% @doc Makes 'access denied' page content.
%% @spec eaccess() -> io_list()
eaccess() ->
    ?MODULE:error("ACCESS DENIED").

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

user_info(Username, UserProperties) ->
    tag("table", ["cellpadding=0", "cellspacing=0"],
        tr(
          string:join(
            [tr(td(b(K ++ ":&nbsp;")) ++ td(V)) ||
                {K, V} <- user_info_cells(Username, UserProperties)]
            , "\n"))).
user_info_cells(Username, UserProperties) ->
    lists:flatmap(
      fun(login) -> [{"Login", Username}];
         (fullname = Key) ->
              [{"Full name",
                case proplists:get_value(Key, UserProperties) of
                    [_ | _] = Value -> Value;
                    _ -> "Not Sure"
                end}];
         (created = Key) ->
              [{"Registered",
                case proplists:get_value(Key, UserProperties) of
                    Value when ?is_now(Value) ->
                        echessd_lib:timestamp(Value);
                    _ -> "unknown"
                end}];
         (_) -> []
      end, [login, fullname, created]).

user_games(Username, UserProperties) ->
    case proplists:get_value(games, UserProperties) of
        [_ | _] = Games ->
            h2("User games:") ++
                string:join(
                  lists:flatmap(
                    fun(GameID) ->
                            case echessd_game:getprops(GameID) of
                                {ok, GameProps} ->
                                    [user_games_(GameID, GameProps)];
                                {error, Reason} ->
                                    echessd_log:err(
                                      "Failed to fetch game #~w props: "
                                      "~9999p (reference from ~9999p user)",
                                      [GameID, Reason, Username]),
                                    []
                            end
                    end, Games), "<br>") ++ "<br>";
        _ -> ""
    end.
user_games_(GameID, GameInfo) ->
    "*&nbsp;" ++ gamelink(GameID) ++ "&nbsp;(" ++
        string:join(
          lists:flatmap(
            fun({Username, Color})
                  when Color == ?white orelse Color == ?black ->
                    [userlink(Username) ++ " " ++ chessman({Color, ?king})];
               (_) -> []
            end, proplists:get_value(users, GameInfo)), ", ") ++ ")" ++
        case get(username) == echessd_game:who_must_turn(GameInfo) of
            true -> " !!!";
            _ -> ""
        end.

html_page_header(Title, Options) ->
    "<html>\n\n"
        "<head>\n"
        "<meta http-equiv='Content-Type' content='text/html; charset=utf-8'>\n"
        "<meta http-equiv='Content-Style-Type' content='text/css'>\n"
        "<meta http-equiv='Content-Script-Type' content='text/javascript'>\n"
        "<title>" ++ Title ++ "</title>\n"
        "<link rel='stylesheet' href='res/styles.css'>\n"
        "</head>\n\n"
        "<body>\n\n" ++
        case [S || {h1, S} <- Options] of
            [H1 | _] -> h1(H1);
            _ -> ""
        end ++
        case get(error) of
            undefined -> "";
            Error ->
                tag("div", ["class=error"],
                    pre("ERROR: " ++ format_error(Error)))
        end.

html_page_footer(_Options) ->
    "\n\n</body>\n"
        "</html>\n".

h1(String)  -> tag("h1", String).
h2(String)  -> tag("h2", String).
b(String)   -> tag("b", String).
tt(String)  -> tag("tt", String).
td(String)  -> tag("td", String).
tr(String)  -> tag("tr", String).
pre(String) -> tag("pre", String).
a(URL, Caption) -> tag("a", ["href='" ++ URL ++ "'"], Caption).

tag(Tag, Value) -> tag(Tag, [], Value).
tag(Tag, Attrs, Value) ->
    "<" ++ Tag ++
        [" " ++ V || V <- Attrs] ++
        ">" ++ Value ++ "</" ++ Tag ++ ">".

userlink(Username) ->
    a("?goto=" ++ ?SECTION_USER ++ "&name=" ++ Username, Username).

gamelink(GameID) ->
    StrID = integer_to_list(GameID),
    a("?goto=" ++ ?SECTION_GAME ++ "&game=" ++ StrID, "#" ++ StrID).

newgame_link(WithUsername) ->
    navig_links(
      [{"?goto=" ++ ?SECTION_NEWGAME++ "&user=" ++ WithUsername,
        "Start new game"}]).

chess_table(GameType, Board, IsRotated) ->
    Letters0 = "abcdefgh",
    Letters =
        if IsRotated -> lists:reverse(Letters0);
           true -> Letters0
        end,
    tag("table", ["cellpadding=0", "cellspacing=0"],
        tr(td("") ++ [tag("td", ["class=crd_t"], tt([C])) ||
                         C <- Letters] ++ td("")) ++
            chess_table_rows(GameType, Board, IsRotated) ++
            tr(td("") ++ [tag("td", ["class=crd_b"], tt([C])) ||
                             C <- Letters] ++ td(""))).

captures([_ | _] = Captures) ->
    tag("table", ["cellpadding=0", "cellspacing=0"],
        case [chessman(F) || {?black, _} = F <- Captures] of
            [_ | _] = Black ->
                tr(
                  tag("td", ["class=crd_l"], "&nbsp;") ++
                      tag("td", ["class=captures"],
                          lists:reverse(Black)));
            _ -> ""
        end ++
        case [chessman(F) || {?white, _} = F <- Captures] of
            [_ | _] = White ->
                tr(
                  tag("td", ["class=crd_l"], "&nbsp;") ++
                      tag("td", ["class=captures"],
                          lists:reverse(White)));
            _ -> ""
        end);
captures(_) -> "".

chess_table_rows(_GameType, Board, false) ->
    chess_table_rows(tuple_to_list(Board), 8, -1, []);
chess_table_rows(GameType, Board, true) ->
    chess_table_rows(
      tuple_to_list(
        echessd_game:transpose(GameType, Board)), 1, 1, []).
chess_table_rows([Row | Tail], N, Step, Result) ->
    StrRow =
        tag("td", ["class=crd_l"], tt(integer_to_list(N))) ++
        chess_table_row(Row, N, Step) ++
        tag("td", ["class=crd_r"], tt(integer_to_list(N))),
    chess_table_rows(Tail, N + Step, Step, [tr(StrRow) | Result]);
chess_table_rows(_, _, _, Result) ->
    lists:reverse(Result).

chess_table_row(Row, N, Step) when is_tuple(Row) ->
    chess_table_row(
      tuple_to_list(Row),
      first_chess_cell_class(N, Step > 0)).
chess_table_row([Chessman | Tail], CellClass) ->
    tag("td", ["class=" ++ CellClass], chessman(Chessman)) ++
        chess_table_row(Tail, next_chess_cell_class(CellClass));
chess_table_row(_, _) -> "".

first_chess_cell_class(Row, false) when Row rem 2 == 0 -> "wc";
first_chess_cell_class(_, false) -> "bc";
first_chess_cell_class(Row, _) ->
    next_chess_cell_class(first_chess_cell_class(Row, false)).

next_chess_cell_class("wc") -> "bc";
next_chess_cell_class("bc") -> "wc".

chessman(?empty) -> "&nbsp;";
chessman(Chessman) ->
    "&#" ++ integer_to_list(chessman_(Chessman)) ++ ";".
chessman_(?wking  ) -> 9812;
chessman_(?wqueen ) -> 9813;
chessman_(?wrook  ) -> 9814;
chessman_(?wbishop) -> 9815;
chessman_(?wknight) -> 9816;
chessman_(?wpawn  ) -> 9817;
chessman_(?bking  ) -> 9818;
chessman_(?bqueen ) -> 9819;
chessman_(?brook  ) -> 9820;
chessman_(?bbishop) -> 9821;
chessman_(?bknight) -> 9822;
chessman_(?bpawn  ) -> 9823.

navig_links(List) -> navig_links(List, undefined).
navig_links([], _Current) -> "";
navig_links(List, Current) ->
    tag("div", ["class=navig"],
        "[&nbsp;" ++
            string:join(
              lists:map(
                fun({URL, Caption}) ->
                        a(URL,
                          if Caption == Current ->
                                  b(Caption);
                             true -> Caption
                          end)
                end, List), "&nbsp;|&nbsp;") ++
            "&nbsp;]").

section_caption(?SECTION_HOME) -> "Home";
section_caption(?SECTION_USERS) -> "Users";
section_caption(Other) -> Other.

navigation() ->
    navig_links(
      [{"?goto=" ++ S, section_caption(S)} ||
          S <- [?SECTION_HOME, ?SECTION_USERS]] ++
          [{"?action=" ++ ?SECTION_EXIT, "Logout"}],
      section_caption(echessd_session:get_val(section))).

game_navigation(GameID) ->
    StrID = integer_to_list(GameID),
    navig_links(
      [{"?goto=" ++ ?SECTION_GAME ++ "&game=" ++ StrID, "Refresh"},
       {"?goto=" ++ ?SECTION_HISTORY ++ "&game=" ++ StrID, "History"}]).

history_navigation(GameID, Step, MaxStep) ->
    StrID = integer_to_list(GameID),
    BaseURL = "?goto=" ++ ?SECTION_HISTORY ++ "&game=" ++ StrID ++ "&step=",
    navig_links(
      if Step > 0 ->
              [{BaseURL ++ integer_to_list(Step - 1), "Previous"}];
         true -> []
      end ++
          if Step < MaxStep ->
                  [{BaseURL ++ integer_to_list(Step + 1), "Next"}];
             true -> []
          end).

formatted_error_page(F, A) ->
    ?MODULE:error(io_lib:format(F, A)).

format_error({error, Reason}) ->
    format_error(Reason);
format_error(Term) ->
    io_lib:format("~120p", [Term]).

