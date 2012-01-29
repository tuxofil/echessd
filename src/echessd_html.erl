%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 21 Jan 2012
%%% @copyright 2012, Aleksey Morarash
%%% @doc HTML page generation functions.

-module(echessd_html).

-export([login/0,
         register/0,
         edituser/0,
         eaccess/0,
         home/0,
         game/1,
         history/1,
         draw_confirm/1,
         giveup_confirm/1,
         users/0,
         user/1,
         newgame/0,
         error/1, error/2,
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
    Timezones =
        [echessd_lib:time_offset_to_list(O) ||
            O <- echessd_lib:administrative_offsets()],
    ServerZone =
        echessd_lib:time_offset_to_list(
          echessd_lib:local_offset()),
    {ok, DefLang} = echessd_cfg:default(?CFG_DEF_LANG),
    html_page_header("echessd - Register new user",
                     [{h1, "echessd register form"}]) ++
        navig_links([{"?goto=" ++ ?SECTION_LOGIN, "Return to login form"}]) ++
        "<form method=post>"
        "<input name=action type=hidden value=register>"
        "Login:    <input name=regusername type=text><br>"
        "Password: <input name=regpassword1 type=password><br>"
        "Confirm password: <input name=regpassword2 type=password><br>"
        "Full name: <input name=regfullname type=text> (optional)<br>"
        "Timezone: <select name=regtimezone>" ++
        lists:map(
          fun(Zone) when Zone == ServerZone ->
                  "<option value='" ++ Zone ++ "' selected>" ++
                      Zone ++ "</option>";
             (Zone) ->
                  "<option value='" ++ Zone ++ "'>" ++ Zone ++ "</option>"
          end, Timezones) ++
        "</select><br>"
        "Language: <select name=reglanguage>" ++
        lists:map(
          fun({LangAbbr, LangName}) ->
                  "<option value='" ++ atom_to_list(LangAbbr) ++ "'" ++
                      if LangAbbr == DefLang ->
                              " selected";
                         true -> ""
                      end ++ ">" ++
                      LangName ++ "</option>"
          end, echessd_lib:languages()) ++
        "</select><br>"
        "<input type=submit value='Register'>"
        "</form>" ++
        html_page_footer([]).

%% @doc Makes 'edit user properties' page content.
%% @spec edituser() -> io_list()
edituser() ->
    Username = get(username),
    {ok, UserInfo} = echessd_user:getprops(Username),
    Fullname =
        echessd_lib:escape_html_entities(
          proplists:get_value(fullname, UserInfo, "")),
    Timezones =
        [echessd_lib:time_offset_to_list(O) ||
            O <- echessd_lib:administrative_offsets()],
    Timezone =
        echessd_lib:time_offset_to_list(
          proplists:get_value(
            timezone, UserInfo,
            echessd_lib:local_offset())),
    {OldLangAbbr, _OldLangName} = echessd_user:lang_info(UserInfo),
    html_page_header(
      "echessd - " ++ gettext(edit_profile_title),
      [{h1, gettext(edit_profile_title)}]) ++
        navigation() ++
        "<br>" ++
        "<form method=post>"
        "<input name=action type=hidden value=" ++ ?SECTION_SAVEUSER ++ ">"
        "Your password: <input name=editpassword0 type=password><br>"
        "New password: <input name=editpassword1 type=password><br>"
        "New password confirm: <input name=editpassword2 type=password><br>"
        "Fullname: <input name=editfullname type=text value='" ++ Fullname ++ "'><br>"
        "Timezone: <select name=edittimezone>" ++
        lists:map(
          fun(Zone) when Zone == Timezone ->
                  "<option value='" ++ Zone ++ "' selected>" ++
                      Zone ++ "</option>";
             (Zone) ->
                  "<option value='" ++ Zone ++ "'>" ++ Zone ++ "</option>"
          end, Timezones) ++
        "</select><br>"
        "Language: <select name=editlanguage>" ++
        lists:map(
          fun({LangAbbr, LangName}) ->
                  "<option value='" ++ atom_to_list(LangAbbr) ++ "'" ++
                      if LangAbbr == OldLangAbbr ->
                              " selected";
                         true -> ""
                      end ++ ">" ++
                      LangName ++ "</option>"
          end, echessd_lib:languages()) ++
        "</select><br>"
        "<input type=submit value=Save>"
        "</form>"
        "<br>" ++
        html_page_footer([]).

%% @doc Makes 'home' page content.
%% @spec home() -> io_list()
home() ->
    Username = get(username),
    {ok, UserInfo} = echessd_user:getprops(Username),
    html_page_header(
      "echessd - Home", [{h1, gettext(home) ++ ": " ++ Username}]) ++
        navigation() ++
        navig_links([{"?goto=" ++ ?SECTION_EDITUSER,
                      gettext(edit_profile_title)}]) ++
        "<br>" ++
        user_info(Username, UserInfo) ++
        "<br>" ++
        user_games(Username, UserInfo, true) ++
        html_page_footer([]).

%% @doc Makes 'registered users list' page content.
%% @spec users() -> io_list()
users() ->
    {ok, Users0} = echessd_user:list(),
    Users = lists:usort(Users0) -- get(username),
    Title = gettext(users),
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
                {ok, UserInfo} ->
                    user(Username, UserInfo);
                {error, Reason} ->
                    ?MODULE:error(
                       "Unable to fetch user ~9999p properties:~n~p",
                       [Username, Reason])
            end
    end.
user(Username, UserInfo) ->
    Title = gettext(user) ++ " '" ++ Username ++ "'",
    html_page_header("echessd - " ++ Title, [{h1, Title}]) ++
        navigation() ++
        "<br>" ++
        user_info(Username, UserInfo) ++
        "<br>" ++
        user_games(Username, UserInfo, false) ++
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
            ?MODULE:error(
               "Unable to fetch user ~9999p properties:~n~p",
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
    case echessd_game:fetch(GameID) of
        {ok, GameInfo, {Board, Captures}} ->
            case proplists:get_value(acknowledged, GameInfo) of
                true ->
                    game(GameID, GameInfo, Board, Captures);
                _ ->
                    ?MODULE:error(
                       "Game #~w isn`t confirmed yet!", [GameID])
            end;
        {error, Reason} ->
            ?MODULE:error(
               "Unable to fetch game #~9999p properties:~n~p",
               [GameID, Reason])
    end.
game(GameID, GameInfo, Board, Captures) ->
    Iam = get(username),
    GameType = proplists:get_value(type, GameInfo),
    Players =
        [I || {users, L} <- GameInfo, {_, C} = I <- L,
              lists:member(C, [?black, ?white])],
    Users = lists:usort([N || {N, _} <- Players]),
    IsMyGame = lists:member(Iam, Users),
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
                IsMyGame andalso
                    lists:member(?black, MyColors)
        end,
    LastPly =
        case lists:reverse(
               proplists:get_value(moves, GameInfo, [])) of
            [LastPly0 | _] -> LastPly0;
            _ -> undefined
        end,
    GameStatus = proplists:get_value(status, GameInfo, none),
    Winner = proplists:get_value(winner, GameInfo),
    html_page_header(
      "echessd - " ++ gettext(game),
      [{h1, gettext(game) ++ " #" ++ integer_to_list(GameID)}]) ++
        navigation() ++
        game_navigation(GameID, IsMyGame andalso GameStatus == none) ++
        case GameStatus of
            checkmate ->
                h2("Game over: checkmate, winner: " ++
                       userlink(Winner));
            give_up ->
                h2("Game over: gived up, winner: " ++
                       userlink(Winner));
            {draw, DrawType} ->
                h2("Game over: draw (" ++
                       atom_to_list(DrawType) ++ ")");
            _ ->
                case proplists:get_value(
                       draw_request_from, GameInfo) of
                    Iam when IsMyGame ->
                        tag("div", ["class=warning"],
                            "You have been proposed draw...");
                    OppUser when IsMyGame ->
                        tag("div", ["class=warning"],
                            userlink(OppUser) ++ " is proposing draw");
                    _ -> ""
                end
        end ++
        chess_table(GameType, Board, IsRotated, LastPly) ++
        case TurnUser of
            Iam when GameStatus == none ->
                "<form method=post>"
                    "Move:&nbsp;"
                    "<input name=action type=hidden value=move>"
                    "<input name=game type=hidden value=" ++
                    integer_to_list(GameID) ++ ">"
                    "<input name=move type=text size=4>"
                    "<input type=submit value=OK>"
                    "<input type=reset value=Reset>"
                    "</form><br>";
            _ -> ""
        end ++
        captures(Captures) ++
        html_page_footer([]).

%% @doc Makes 'game history' page content.
%% @spec history(GameID) -> io_list()
%%     GameID = echessd_game:echessd_game_id()
history(GameID) ->
    case echessd_game:getprops(GameID) of
        {ok, GameInfo} ->
            GameType = proplists:get_value(type, GameInfo),
            FullHistory = proplists:get_value(moves, GameInfo, []),
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
            LastPly =
                case lists:reverse(History) of
                    [LastPly0 | _] -> LastPly0;
                    _ -> undefined
                end,
            html_page_header(
              "echessd - Game history",
              [{h1, "Game " ++ gamelink(GameID) ++ " history"}]) ++
                navigation() ++
                history_navigation(GameID, Step, FullHistoryLen) ++
                chess_table(GameType, Board, false, LastPly) ++
                captures(Captures) ++
                html_page_footer([]);
        {error, Reason} ->
            ?MODULE:error(
               "Unable to fetch game #~9999p properties:~n~p",
               [GameID, Reason])
    end.

%% @doc Makes 'draw confirmation' page content.
%% @spec draw_confirm(GameID) -> io_list()
%%     GameID = echessd_game:echessd_game_id()
draw_confirm(GameID) ->
    html_page_header("echessd - Game draw by agreement confirmation",
                     [{h1, "You are about tie proposal!"}]) ++
        tag("div", ["class=warning"],
            "Do you want to propose to end the game with "
            "draw by agreement?") ++
        "<form method=post>"
        "<input type=hidden name=action value=" ++ ?SECTION_DRAW ++ ">"
        "<input type=hidden name=game value=" ++
        integer_to_list(GameID) ++ ">"
        "<input type=submit value='Propose draw'>"
        "</form>" ++
        navig_links([{"javascript: history.back();",
                      gettext(ouch_back_link)}]) ++
        html_page_footer([]).

%% @doc Makes 'giving up confirmation' page content.
%% @spec giveup_confirm(GameID) -> io_list()
%%     GameID = echessd_game:echessd_game_id()
giveup_confirm(GameID) ->
    Iam = get(username),
    {ok, GameInfo} = echessd_game:getprops(GameID),
    Players =
        [N || {users, L} <- GameInfo,
              {N, C} <- L, lists:member(C, [?white, ?black])],
    [Opponent | _] = Players -- [Iam],
    html_page_header("echessd - Game give up confirmation",
                     [{h1, "You are about giving up!"}]) ++
        tag("div", ["class=warning"],
            "Do you wish to '" ++ userlink(Opponent) ++
                "' to win this game?") ++
        "<form method=post>"
        "<input type=hidden name=action value=" ++ ?SECTION_GIVEUP ++ ">"
        "<input type=hidden name=game value=" ++
        integer_to_list(GameID) ++ ">"
        "<input type=submit value='Give up'>"
        "</form>" ++
        navig_links([{"javascript: history.back();",
                      gettext(ouch_back_link)}]) ++
        html_page_footer([]).

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
        tag("div", ["class=error"], pre(Message)) ++
        "<br>" ++
        navig_links([{"javascript: history.back();", gettext(back_link)}]) ++
        html_page_footer([]).

%% @doc Makes 'error' page content.
%% @spec error(Format, Args) -> io_list()
%%     Format = string(),
%%     Args = list()
error(Format, Args) ->
    ?MODULE:error(io_lib:format(Format, Args)).

%% @doc Makes 'access denied' page content.
%% @spec eaccess() -> io_list()
eaccess() ->
    ?MODULE:error("ACCESS DENIED").

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

user_info(Username, UserInfo) ->
    tag("table", ["cellpadding=0", "cellspacing=0"],
        tr(
          string:join(
            [tr(td(b(K ++ ":&nbsp;")) ++ td(V)) ||
                {K, V} <- user_info_cells(Username, UserInfo)]
            , "\n"))).
user_info_cells(Username, UserInfo) ->
    LangInfo = echessd_user:lang_info(UserInfo),
    lists:flatmap(
      fun(login) -> [{gettext(login), Username}];
         (fullname = Key) ->
              [{gettext(fullname),
                case proplists:get_value(Key, UserInfo) of
                    [_ | _] = Value ->
                        echessd_lib:escape_html_entities(Value);
                    _ -> gettext(not_sure)
                end}];
         (created = Key) ->
              [{gettext(registered),
                case proplists:get_value(Key, UserInfo) of
                    Value when ?is_now(Value) ->
                        echessd_lib:timestamp(
                          Value, get(timezone));
                    _ -> gettext(unknown)
                end}];
         (timezone = Key) ->
              [{gettext(timezone),
                case proplists:get_value(Key, UserInfo) of
                    Value when is_tuple(Value) ->
                        echessd_lib:time_offset_to_list(Value);
                    _ ->
                        echessd_lib:time_offset_to_list(
                          echessd_lib:local_offset())
                end}];
         (language) ->
              {_LangAbbr, LangName} = LangInfo,
              [{gettext(language), LangName}];
         (_) -> []
      end, [login, fullname, created, timezone, language]).

user_games(Username, UserInfo, ShowNotAcknowledged) ->
    %% fetch all user games info
    UserGames =
        lists:flatmap(
          fun(GameID) ->
                  case echessd_game:getprops(GameID) of
                      {ok, GameInfo} ->
                          [{GameID, GameInfo}];
                      {error, Reason} ->
                          echessd_log:err(
                            "Failed to fetch game #~w props: "
                            "~9999p (reference from ~9999p user)",
                            [GameID, Reason, Username]),
                          []
                  end
          end, proplists:get_value(games, UserInfo, [])),
    %% split not acknowledged
    {Confirmed, NotConfirmed} =
        lists:partition(
          fun({_GameID, GameInfo}) ->
                  proplists:get_value(acknowledged, GameInfo)
          end, UserGames),
    case Confirmed of
        [_ | _] ->
            h2(gettext(user_games) ++ ":") ++
                string:join(
                  [user_game_(Username, I, L) ||
                      {I, L} <- Confirmed], "<br>") ++
                "<br>";
        _ -> ""
    end ++
        case NotConfirmed of
            [_ | _] when ShowNotAcknowledged ->
                h2(gettext(unconf_games) ++ ":") ++
                    string:join(
                      [user_unconfirmed_game_(Username, I, L) ||
                          {I, L} <- NotConfirmed], "<br>") ++
                    "<br>";
            _ -> ""
        end.

user_game_(Owner, GameID, GameInfo) ->
    GamePlayers =
        [{N, C} || {N, C} <- proplists:get_value(users, GameInfo, []),
                   lists:member(C, [?white, ?black])],
    UniquePlayerNames = lists:usort([N || {N, _} <- GamePlayers]),
    IsTest = length(UniquePlayerNames) == 1,
    "* " ++ gamelink(GameID) ++
        if IsTest -> " " ++ gettext(test_game);
           true ->
                Color = proplists:get_value(Owner, GamePlayers),
                Opponent = hd(UniquePlayerNames -- [Owner]),
                OpponentColor = proplists:get_value(Opponent, GamePlayers),
                " " ++ chessman({Color, ?king}) ++ " " ++ gettext(vs) ++
                    " " ++ userlink(Opponent) ++ " " ++
                    chessman({OpponentColor, ?king})
        end ++
        case proplists:get_value(status, GameInfo) of
            none ->
                case get(username) == echessd_game:who_must_turn(GameInfo) of
                    true when not IsTest -> " !!!";
                    _ -> ""
                end;
            checkmate when IsTest -> " - " ++ gettext(checkmate);
            checkmate ->
                case proplists:get_value(winner, GameInfo) of
                    Owner -> " - " ++ gettext(win);
                    _ -> " - " ++ gettext(loose)
                end;
            give_up when IsTest -> " - " ++ gettext(gived_up);
            give_up ->
                case proplists:get_value(winner, GameInfo) of
                    Owner -> " - " ++ gettext(win_giveup);
                    _ -> " - " ++ gettext(loose_giveup)
                end;
            {draw, _} ->
                " - " ++ gettext(draw)
        end.

user_unconfirmed_game_(Owner, GameID, GameInfo) ->
    StrGameID = integer_to_list(GameID),
    GamePlayers =
        [{N, C} || {N, C} <- proplists:get_value(users, GameInfo, []),
                   lists:member(C, [?white, ?black])],
    UniquePlayerNames = lists:usort([N || {N, _} <- GamePlayers]),
    "* #" ++ StrGameID ++
        case proplists:get_value(creator, GameInfo) of
            Owner ->
                Opponent = hd(UniquePlayerNames -- [Owner]),
                OpponentColor = proplists:get_value(Opponent, GamePlayers),
                " waiting for " ++ userlink(Opponent) ++ " " ++
                    chessman({OpponentColor, ?king}) ++ "...";
            Opponent ->
                OpponentColor = proplists:get_value(Opponent, GamePlayers),
                AckURL =
                    "?action=" ++ ?SECTION_ACKGAME ++ "&game=" ++ StrGameID,
                " " ++ userlink(Opponent) ++ " " ++
                    chessman({OpponentColor, ?king}) ++
                    " is waiting for you! " ++
                    tag("a", ["href='" ++ AckURL ++ "'"], "Confirm")
        end ++ " " ++
        tag("a", ["href='" ++ "?action=" ++ ?SECTION_DENYGAME ++
                      "&game=" ++ StrGameID ++"'"], "Deny").

html_page_header(Title, Options) ->
    "<html>\n\n"
        "<head>\n"
        "<meta http-equiv='Content-Type' content='text/html; charset=utf-8'>\n"
        "<meta http-equiv='Content-Style-Type' content='text/css'>\n"
        "<meta http-equiv='Content-Script-Type' content='text/javascript'>\n"
        "<title>" ++ Title ++ "</title>\n"
        "<link rel='stylesheet' href='/res/styles.css'>\n"
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
                    pre(gettext(error) ++ ": " ++ format_error(Error)))
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
        gettext(new_game_link)}]).

chess_table(GameType, Board, IsRotated, LastPly) ->
    Letters0 = "abcdefgh",
    Letters =
        if IsRotated -> lists:reverse(Letters0);
           true -> Letters0
        end,
    put(is_rotated, IsRotated),
    tag("table", ["cellpadding=0", "cellspacing=0"],
        tr(td("") ++ [tag("td", ["class=crd_t"], tt([C])) ||
                         C <- Letters] ++ td("")) ++
            chess_table_rows(GameType, Board, IsRotated, LastPly) ++
            tr(td("") ++ [tag("td", ["class=crd_b"], tt([C])) ||
                             C <- Letters] ++ td(""))).

chess_table_rows(_GameType, Board, false, LastPly) ->
    chess_table_rows_(tuple_to_list(Board), 8, -1, LastPly, []);
chess_table_rows(GameType, Board, true, LastPly) ->
    chess_table_rows_(
      tuple_to_list(
        echessd_game:transpose(GameType, Board)), 1, 1, LastPly, []).
chess_table_rows_([Row | Tail], N, Step, LastPly, Result) ->
    StrRow =
        tag("td", ["class=crd_l"], tt(integer_to_list(N))) ++
        chess_table_row(Row, N, Step, LastPly) ++
        tag("td", ["class=crd_r"], tt(integer_to_list(N))),
    chess_table_rows_(Tail, N + Step, Step, LastPly, [tr(StrRow) | Result]);
chess_table_rows_(_, _, _, _, Result) ->
    lists:reverse(Result).

chess_table_row(Row, N, Step, LastPly) when is_tuple(Row) ->
    put(row_index, N),
    erase(col_index),
    chess_table_row_(
      tuple_to_list(Row),
      first_chess_cell_class(N, Step > 0), LastPly).
chess_table_row_([Chessman | Tail], CellClass, LastPly) ->
    Col =
        case get(col_index) of
            Col0 when is_integer(Col0) ->
                put(col_index, Col0 + 1), Col0;
            _ ->
                put(col_index, 2), 1
        end,
    ExtraAttrs =
        case LastPly of
            [A, B, C, D | _] ->
                CurInd =
                    [case get(is_rotated) of
                         true -> $a - Col + 8;
                         _ -> $a + Col - 1
                     end, $1 + get(row_index) - 1],
                if [A, B] == CurInd orelse [C, D] == CurInd ->
                        ["style='border-style:solid;'"];
                   true -> []
                end;
            _ -> []
        end,
    tag("td", ["class=" ++ CellClass] ++ ExtraAttrs, chessman(Chessman)) ++
        chess_table_row_(Tail, next_chess_cell_class(CellClass), LastPly);
chess_table_row_(_, _, _) -> "".

first_chess_cell_class(Row, false) when Row rem 2 == 0 -> "wc";
first_chess_cell_class(_, false) -> "bc";
first_chess_cell_class(Row, _) ->
    next_chess_cell_class(first_chess_cell_class(Row, false)).

next_chess_cell_class("wc") -> "bc";
next_chess_cell_class("bc") -> "wc".

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
                fun({[_ | _] = URL, [_ | _] = Caption}) ->
                        a(URL,
                          if Caption == Current ->
                                  b(Caption);
                             true -> Caption
                          end);
                   ({_, [_ | _] = Caption}) ->
                        if Caption == Current -> b(Caption);
                           true -> Caption
                        end
                end, List), "&nbsp;|&nbsp;") ++
            "&nbsp;]").

section_caption(?SECTION_HOME) -> gettext(home);
section_caption(?SECTION_USERS) -> gettext(users);
section_caption(Other) -> Other.

navigation() ->
    navig_links(
      [{"?goto=" ++ S, section_caption(S)} ||
          S <- [?SECTION_HOME, ?SECTION_USERS]] ++
          [{"?action=" ++ ?SECTION_EXIT, gettext(logout)}],
      section_caption(echessd_session:get_val(section))).

game_navigation(GameID, ShowEndGameLinks) ->
    StrID = integer_to_list(GameID),
    navig_links(
      [{"?goto=" ++ ?SECTION_GAME ++ "&game=" ++ StrID, gettext(refresh)},
       {"?goto=" ++ ?SECTION_HISTORY ++ "&game=" ++ StrID, gettext(history)}] ++
          if ShowEndGameLinks ->
                  [{"?goto=" ++ ?SECTION_DRAW_CONFIRM ++
                        "&game=" ++ StrID, gettext(req_draw)},
                   {"?goto=" ++ ?SECTION_GIVEUP_CONFIRM ++
                        "&game=" ++ StrID, gettext(do_giveup)}];
             true -> []
          end).

history_navigation(GameID, Step, MaxStep) ->
    StrID = integer_to_list(GameID),
    BaseURL = "?goto=" ++ ?SECTION_HISTORY ++ "&game=" ++ StrID ++ "&step=",
    navig_links(
      if Step > 0 ->
              [{BaseURL ++ integer_to_list(Step - 1), gettext(prev)}];
         true -> [{undefined, gettext(prev)}]
      end ++
          if Step < MaxStep ->
                  [{BaseURL ++ integer_to_list(Step + 1), gettext(next)}];
             true -> [{undefined, gettext(next)}]
          end).

format_error({error, Reason}) ->
    format_error(Reason);
format_error({wrong_move, Reason}) ->
    gettext(badmove) ++
        case Reason of
            check -> gettext(king_in_check);
            badmove -> "";
            friendly_fire -> "";
            _ -> format_error(Reason)
        end;
format_error(Term) ->
    io_lib:format("~120p", [Term]).

gettext(TextID) ->
    echessd_lib:gettext(TextID, get(language)).

