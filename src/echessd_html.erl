%%%-------------------------------------------------------------------
%%% File    : echessd_html.erl
%%% Author  : Aleksey Morarash <aleksey.morarash@gmail.com>
%%% Created : 20 Jan 2012
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
         users/0,
         user/1,
         newgame/0,
         notyet/0
        ]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

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

register() ->
    html_page_header("echessd - Register new user",
                     [{h1, "echessd register form"}]) ++
        navig_links([{"?goto=" ++ ?SECTION_LOGIN, "Return to login form"}]) ++
        "<form method=post>"
        "<input name=action type=hidden value=register>"
        "Login:    <input name=regusername type=text><br>"
        "Password: <input name=regpassword1 type=password><br>"
        "Confirm password: <input name=regpassword2 type=password><br>"
        "<input type=submit value='Register'>"
        "</form>" ++
        html_page_footer([]).

error(String) ->
    html_page_header("echessd - Error", [{h1, "echessd error"}]) ++
        tag("div", ["class=error"], String) ++
        "<br>" ++
        navig_links([{"javascript: history.back();", "Back"}]) ++
        html_page_footer([]).

eaccess() -> ?MODULE:error("ACCESS DENIED").

home() ->
    User = get(username),
    {ok, UserInfo} = echessd_user:getprops(User),
    html_page_header(
      "echessd - Home", [{h1, "Logged in as '" ++ User ++ "'"}]) ++
        navigation() ++
        "<br>" ++
        user_info(User, UserInfo) ++
        "<br>" ++
        user_games(User, UserInfo) ++
        "<br>" ++
        newgame_link(User) ++
        html_page_footer([]).

users() ->
    {ok, Users0} = echessd_user:list(),
    Users = lists:usort(Users0) -- get(username),
    Title = "User list",
    html_page_header("echessd - " ++ Title, [{h1, Title}]) ++
        navigation() ++
        "<br>" ++
        string:join(
          lists:map(
            fun(User) ->
                    "*&nbsp;" ++ userlink(User)
            end, Users), "<br>") ++
        html_page_footer([]).

user(User) ->
    case get(username) of
        User -> home();
        _ ->
            case echessd_user:getprops(User) of
                {ok, UserInfo} ->
                    user(User, UserInfo);
                {error, Reason} ->
                    formatted_error_page(
                      "Unable to fetch user ~9999p properties:<br>" ++ tt("~p"),
                      [User, Reason])
            end
    end.
user(User, UserInfo) ->
    Title = "User '" ++ User ++ "'",
    html_page_header("echessd - " ++ Title, [{h1, Title}]) ++
        navigation() ++
        "<br>" ++
        user_info(User, UserInfo) ++
        "<br>" ++
        user_games(User, UserInfo) ++
        "<br>" ++
        newgame_link(User) ++
        html_page_footer([]).

newgame() ->
    Opponent = proplists:get_value("user", get(query_proplist)),
    case echessd_user:getprops(Opponent) of
        {ok, OpponentInfo} ->
            newgame(Opponent, OpponentInfo);
        {error, Reason} ->
            formatted_error_page(
              "Unable to fetch user ~9999p properties:<br>" ++ tt("~p"),
              [Opponent, Reason])
    end.
newgame(Opponent, OpponentInfo) ->
    echessd_session:set_val(opponent, {Opponent, OpponentInfo}),
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
        "Game type: <select name=gametype>"
        "<option value='classic'>Classic chess</option>"
        "</select><br>"
        ++ ColorSelector ++
        "<input type=submit value='Create'>"
        "</form>" ++
        html_page_footer([]).

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
                navig_links(
                  [{"?goto=" ++ ?SECTION_GAME ++
                        "&game=" ++ integer_to_list(GameID), "Refresh"}]) ++
                chess_table(GameType, Board, Captures, IsRotated) ++
                case TurnUser of
                    Iam ->
                        "<form method=post>"
                            "Move:&nbsp;"
                            "<input name=action type=hidden value=move>"
                            "<input name=game type=hidden value=" ++
                            integer_to_list(GameID) ++ ">"
                            "<input name=move type=text size=4>"
                            "<input type=submit value=Move>"
                            "</form>";
                    _ -> ""
                end ++
                html_page_footer([]);
        {error, Reason} ->
            formatted_error_page(
              "Unable to fetch game #~9999p properties:<br>" ++ tt("~p"),
              [GameID, Reason])
    end.

notyet() ->
    html_page_header("echessd - Under construction",
                     [{h1, "Not implemented yet"}]) ++
        navig_links([{"javascript: history.back();", "Back"}]) ++
        html_page_footer([]).

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

user_info(User, UserInfo) ->
    "<table cellpadding=0 cellspacing=0><tr>\n" ++
        string:join(
          [tr(td(b(K ++ ":&nbsp;")) ++ td(V)) ||
              {K, V} <- user_info_cells(User, UserInfo)]
          , "\n") ++
        "</tr></table>\n".
user_info_cells(User, UserInfo) ->
    lists:flatmap(
      fun(login) -> [{"Login", User}];
         (fullname = Key) ->
              [{"Full name",
                case proplists:get_value(Key, UserInfo) of
                    [_ | _] = Value -> Value;
                    _ -> "Not Sure"
                end}];
         (created = Key) ->
              [{"Registered",
                case proplists:get_value(Key, UserInfo) of
                    Value when ?is_now(Value) ->
                        echessd_lib:timestamp(Value);
                    _ -> "unknown"
                end}];
         (_) -> []
      end, [login, fullname, created]).

user_games(User, UserInfo) ->
    case proplists:get_value(games, UserInfo) of
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
                                      [GameID, Reason, User]),
                                    []
                            end
                    end, Games), "<br>") ++ "<br>";
        _ -> ""
    end.
user_games_(Game, GameInfo) ->
    "*&nbsp;" ++ gamelink(Game) ++ "&nbsp;(" ++
        string:join(
          lists:flatmap(
            fun({User, Color}) when Color == ?white orelse Color == ?black ->
                    [userlink(User) ++ " " ++ figure({Color, ?king})];
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
                tag("div", ["class=error"], pre(format_error(Error)))
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

userlink(User) ->
    a("?goto=" ++ ?SECTION_USER ++ "&name=" ++ User, User).

gamelink(Game) ->
    StrID = integer_to_list(Game),
    a("?goto=" ++ ?SECTION_GAME ++ "&game=" ++ StrID, "#" ++ StrID).

newgame_link(WithUser) ->
    navig_links([{"?goto=" ++ ?SECTION_NEWGAME++ "&user=" ++ WithUser,
                  "Start new game"}]).

chess_table(GameType, Board, Captures, IsRotated) ->
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
                             C <- Letters] ++ td(""))) ++
        captures(Captures).
captures([_ | _] = Captures) ->
    tag("table", ["cellpadding=0", "cellspacing=0"],
        case [figure(F) || {?black, _} = F <- Captures] of
            [_ | _] = Black ->
                tr(tag("td", ["class=captured"],
                       lists:reverse(Black)));
            _ -> ""
        end ++
        case [figure(F) || {?white, _} = F <- Captures] of
            [_ | _] = White ->
                tr(tag("td", ["class=captured"],
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
chess_table_row([Figure | Tail], CellClass) ->
    tag("td", ["class=" ++ CellClass], figure(Figure)) ++
        chess_table_row(Tail, next_chess_cell_class(CellClass));
chess_table_row(_, _) -> "".

first_chess_cell_class(Row, false) when Row rem 2 == 0 -> "wc";
first_chess_cell_class(_, false) -> "bc";
first_chess_cell_class(Row, _) ->
    next_chess_cell_class(first_chess_cell_class(Row, false)).

next_chess_cell_class("wc") -> "bc";
next_chess_cell_class("bc") -> "wc".

figure(?empty) -> "&nbsp;";
figure(Fig) ->
    "&#" ++ integer_to_list(figure_(Fig)) ++ ";".
figure_(?wking  ) -> 9812;
figure_(?wqueen ) -> 9813;
figure_(?wrook  ) -> 9814;
figure_(?wbishop) -> 9815;
figure_(?wknight) -> 9816;
figure_(?wpawn  ) -> 9817;
figure_(?bking  ) -> 9818;
figure_(?bqueen ) -> 9819;
figure_(?brook  ) -> 9820;
figure_(?bbishop) -> 9821;
figure_(?bknight) -> 9822;
figure_(?bpawn  ) -> 9823.

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

formatted_error_page(F, A) ->
    ?MODULE:error(io_lib:format(F, A)).

format_error({error, Reason}) ->
    format_error(Reason);
format_error(Term) ->
    io_lib:format("~p", [Term]).

