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
         test_table/1,
         notyet/0
        ]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

login() ->
    header("echessd - Login", []) ++
        h1("echessd login") ++
        navig_links([{"?goto=" ++ ?SECTION_REG, "Register new user"}]) ++
        "<form method=post>"
        "<input name=action type=hidden value=login>"
        "Login:    <input name=username type=text><br>"
        "Password: <input name=password type=password><br>"
        "<input type=submit value='Login'>"
        "</form>" ++
        footer([]).

register() ->
    header("echessd - Register new user", []) ++
        h1("echessd register form") ++
        navig_links([{"?goto=" ++ ?SECTION_LOGIN, "Return to login form"}]) ++
        "<form method=post>"
        "<input name=action type=hidden value=register>"
        "Login:    <input name=regusername type=text><br>"
        "Password: <input name=regpassword1 type=password><br>"
        "Confirm password: <input name=regpassword2 type=password><br>"
        "<input type=submit value='Register'>"
        "</form>" ++
        footer([]).

error(String) ->
    header("echessd - Error", []) ++
        h1("echessd error") ++
        "<div class=error>" ++ String ++ "</div>"
        "<br>" ++
        navig_links([{"javascript: history.back();", "Back"}]) ++
        footer([]).

eaccess() -> ?MODULE:error("ACCESS DENIED").

home() ->
    User = get(username),
    {ok, UserInfo} = echessd_user:getprops(User),
    header("echessd - Home", []) ++
        h1("Logged in as \"" ++ User ++ "\"") ++
        navigation() ++
        "<br>" ++
        user_info(User, UserInfo) ++
        "<br>" ++
        user_games(User, UserInfo) ++
        "<br>" ++
        newgame_link(User) ++
        footer([]).

users() ->
    {ok, Users0} = echessd_user:list(),
    Users = lists:usort(Users0) -- get(username),
    header("echessd - Users list", []) ++
        h1("Users list") ++
        navigation() ++
        "<br>" ++
        string:join(
          lists:map(
            fun(User) ->
                    "*&nbsp;" ++ userlink(User)
            end, Users), "<br>") ++
        footer([]).

user(User) ->
    case get(username) of
        User -> home();
        _ ->
            case echessd_user:getprops(User) of
                {ok, UserInfo} ->
                    user(User, UserInfo);
                {error, Reason} ->
                    format_error(
                      "Unable to fetch user ~9999p properties:<br>" ++ tt("~p"),
                      [User, Reason])
            end
    end.
user(User, UserInfo) ->
    header("echessd - User '" ++ User ++ "'", []) ++
        h1("User '" ++ User ++ "'") ++
        navigation() ++
        "<br>" ++
        user_info(User, UserInfo) ++
        "<br>" ++
        user_games(User, UserInfo) ++
        "<br>" ++
        newgame_link(User) ++
        footer([]).

newgame() ->
    Iam = get(username),
    case proplists:get_value("user", get(query_proplist)) of
        Iam ->
            format_error("You cannot play with yourself at now!", []);
        User ->
            case echessd_user:getprops(User) of
                {ok, UserInfo} ->
                    newgame(User, UserInfo);
                {error, Reason} ->
                    format_error(
                      "Unable to fetch user ~9999p properties:<br>" ++ tt("~p"),
                      [User, Reason])
            end
    end.
newgame(User, UserInfo) ->
    echessd_session:set_val(opponent, {User, UserInfo}),
    header("echessd - New game", []) ++
        h1("New game") ++
        navigation() ++
        h2("Your opponent: '" ++ User ++ "'") ++
        "<form method=post>"
        "<input name=action type=hidden value=" ++ ?SECTION_NEWGAME ++ ">"
        "Game type: <select name=gametype>"
        "<option value='classic'>Classic chess</option>"
        "</select><br>"
        "Color: <select name=color>"
        "<option value='random'>Choose randomly</option>"
        "<option value='white'>White</option>"
        "<option value='black'>Black</option>"
        "</select><br>"
        "<input type=submit value='Create'>"
        "</form>" ++
        footer([]).

game(GameID) ->
    Iam = get(username),
    case echessd_game:fetch(GameID) of
        {ok, GameInfo, {Table, Tooked}} ->
            MyColor =
                proplists:get_value(
                  Iam, proplists:get_value(users, GameInfo, [])),
            TurnUser = echessd_game:who_must_turn(GameInfo),
            IsRotated = MyColor == ?black,
            header("echessd - Game", []) ++
                h1("Game #" ++ integer_to_list(GameID)) ++
                navigation() ++
                navig_links(
                  [{"?goto=" ++ ?SECTION_GAME ++
                        "&game=" ++ integer_to_list(GameID), "Refresh"}]) ++
                table(Table, Tooked, IsRotated) ++
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
                footer([]);
        {error, Reason} ->
            format_error(
              "Unable to fetch game #~9999p properties:<br>" ++ tt("~p"),
              [GameID, Reason])
    end.

test_table(Moves) ->
    Game0 = echessd_game:new(?GAME_CLASSIC),
    {Game, Tooked} =
        lists:foldl(
          fun(Move, {G0, T0}) ->
                  {G, T} = echessd_game:move(G0, Move),
                  {G, [T | T0]}
          end, {Game0, []}, Moves),
    header("echessd - Test table", []) ++
        h1("Test table") ++
        navigation() ++
        table(Game, Tooked, false) ++
        footer([]).

notyet() ->
    header("echessd - Under construction", []) ++
        h1("Not implemented yet") ++
        "" ++
        navig_links([{"javascript: history.back();", "Back"}]) ++
        footer([]).

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
                    fun(Game) ->
                            case echessd_game:getprops(Game) of
                                {ok, GameProps} ->
                                    [user_games_(Game, GameProps)];
                                {error, Reason} ->
                                    echessd_log:err(
                                      "Failed to fetch game #~w props: "
                                      "~9999p (reference from ~9999p user)",
                                      [Game, Reason, User]),
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
                    [userlink(User) ++ " as " ++
                         if Color == ?white -> "white";
                            true -> "black"
                         end];
               (_) -> []
            end, proplists:get_value(users, GameInfo)), ", ") ++ ")".

userlink(User) ->
    "<a href='?goto=" ++ ?SECTION_USER ++
        "&name=" ++ User ++ "'>" ++ User ++ "</a>".

gamelink(Game) ->
    StrID = integer_to_list(Game),
    "<a href='?goto=" ++ ?SECTION_GAME ++
        "&game=" ++ StrID ++ "'>#" ++ StrID ++ "</a>".

newgame_link(WithUser) ->
    navig_links([{"?goto=" ++ ?SECTION_NEWGAME++ "&user=" ++ WithUser,
                  "Start new game"}]).

header(Title, _Options) ->
    "<html>\n\n"
        "<head>\n"
        "<meta http-equiv='Content-Type' content='text/html; charset=utf-8'>\n"
        "<meta http-equiv='Content-Style-Type' content='text/css'>\n"
        "<meta http-equiv='Content-Script-Type' content='text/javascript'>\n"
        "<title>" ++ Title ++ "</title>\n"
        "<link rel='stylesheet' href='res/styles.css'>\n"
        "</head>\n\n"
        "<body>\n\n".

footer(_Options) ->
    "\n\n</body>\n"
        "</html>\n".

h1(String) ->
    "<h1>" ++ String ++ "</h1>".

h2(String) ->
    "<h2>" ++ String ++ "</h2>".

b(String) ->
    "<b>" ++ String ++ "</b>".

tt(String) ->
    "<tt>" ++ String ++ "</tt>".

td(String) ->
    "<td>" ++ String ++ "</td>".

tr(String) ->
    "<tr>" ++ String ++ "</tr>".

tag(Tag, Attrs, Value) ->
    "<" ++ Tag ++
        [" " ++ V || V <- Attrs] ++
        ">" ++ Value ++ "</" ++ Tag ++ ">".

table(Game, Tooked, IsRotated) ->
    Letters0 = "abcdefgh",
    Letters =
        if IsRotated -> lists:reverse(Letters0);
           true -> Letters0
        end,
    "<table cellpadding=0 cellspacing=0>\n" ++
        tr(td("") ++ [tag("td", ["class=crd_t"], tt([C])) ||
                         C <- Letters] ++ td("")) ++
        table_rows(Game, IsRotated) ++
        tr(td("") ++ [tag("td", ["class=crd_b"], tt([C])) ||
                         C <- Letters] ++ td("")) ++
        "</table>\n" ++
        tooked(Tooked).
tooked([_ | _] = Tooked) ->
    "<table cellpadding=0 cellspacing=0>\n" ++
        case [figure(F) || {?black, _} = F <- Tooked] of
            [_ | _] = Black ->
                tr(tag("td", ["class=tooked"],
                       lists:reverse(Black)));
            _ -> ""
        end ++
        case [figure(F) || {?white, _} = F <- Tooked] of
            [_ | _] = White ->
                tr(tag("td", ["class=tooked"],
                       lists:reverse(White)));
            _ -> ""
        end ++
        "</table>";
tooked(_) -> "".

table_rows(Game, false) ->
    table_rows(tuple_to_list(Game), 8, -1, []);
table_rows(Game, true) ->
    table_rows(tuple_to_list(echessd_game:transpose(Game)), 1, 1, []).
table_rows([Row | Tail], N, Step, Result) ->
    StrRow =
        tag("td", ["class=crd_l"], tt(integer_to_list(N))) ++
        table_row(Row, N, Step) ++
        tag("td", ["class=crd_r"], tt(integer_to_list(N))),
    table_rows(Tail, N + Step, Step, [tr(StrRow) | Result]);
table_rows(_, _, _, Result) ->
    lists:reverse(Result).

table_row(Row, N, Step) when is_tuple(Row) ->
    table_row(
      tuple_to_list(Row),
      first_chess_cell_class(N, Step > 0)).
table_row([Figure | Tail], CellClass) ->
    "<td class=" ++ CellClass ++ ">" ++ figure(Figure) ++ "\n" ++
        table_row(Tail, next_chess_cell_class(CellClass));
table_row(_, _) -> "".

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
    "<div class=navig>[&nbsp;" ++
        string:join(
          lists:map(
            fun({URL, Caption}) ->
                    "<a href='" ++ URL ++ "'>" ++
                        if Caption == Current ->
                                b(Caption);
                           true -> Caption
                        end ++ "</a>"
            end, List), "&nbsp;|&nbsp;") ++
        "&nbsp;]</div>".

section_caption(?SECTION_HOME) -> "Home";
section_caption(?SECTION_USERS) -> "Users";
section_caption(Other) -> Other.

navigation() ->
    navig_links(
      [{"?goto=" ++ S, section_caption(S)} ||
          S <- [?SECTION_HOME, ?SECTION_USERS,
                ?SECTION_TEST]] ++
          [{"?action=" ++ ?SECTION_EXIT, "Logout"}],
      section_caption(echessd_session:get_val(section))).

format_error(F, A) ->
    ?MODULE:error(io_lib:format(F, A)).

