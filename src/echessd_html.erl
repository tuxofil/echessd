-module(echessd_html).

-export([header/2, footer/1,
         h1/1,
         table/1, figure/1,
         login/0,
         register/0,
         error/1,
         eaccess/0,
         main/0,
         games/0,
         game/1,
         users/0,
         test_table/0,
         notyet/0
        ]).

-include("echessd.hrl").

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

table(Game) ->
    "<table cellpadding=0 cellspacing=0>\n"
        "<tr>\n"
        "<td>\n"
        "<td class=crd_t><tt>a</tt>\n"
        "<td class=crd_t><tt>b</tt>\n"
        "<td class=crd_t><tt>c</tt>\n"
        "<td class=crd_t><tt>d</tt>\n"
        "<td class=crd_t><tt>e</tt>\n"
        "<td class=crd_t><tt>f</tt>\n"
        "<td class=crd_t><tt>g</tt>\n"
        "<td class=crd_t><tt>h</tt>\n"
        "<td>\n"
        ++ table_rows(Game) ++
        "<tr>\n"
        "<td>\n"
        "<td class=crd_b><tt>a</tt>\n"
        "<td class=crd_b><tt>b</tt>\n"
        "<td class=crd_b><tt>c</tt>\n"
        "<td class=crd_b><tt>d</tt>\n"
        "<td class=crd_b><tt>e</tt>\n"
        "<td class=crd_b><tt>f</tt>\n"
        "<td class=crd_b><tt>g</tt>\n"
        "<td class=crd_b><tt>h</tt>\n"
        "<td>\n"
        "</table>\n".

table_rows(Game) ->
    table_rows(tuple_to_list(Game), 8, []).
table_rows([Row | Tail], N, Result) ->
    StrRow =
        "<tr>\n"
        "<td class=crd_l><tt>" ++ integer_to_list(N) ++ "</tt>\n"
        ++ table_row(Row, N) ++
        "<td class=crd_r><tt>" ++ integer_to_list(N) ++ "</tt>\n",
    table_rows(Tail, N - 1, [StrRow | Result]);
table_rows(_, _, Result) ->
    lists:reverse(Result).

table_row(Row, N) when is_tuple(Row) ->
    table_row(
      tuple_to_list(Row),
      if N rem 2 == 0 -> "wc";
         true -> "bc"
      end);
table_row([Figure | Tail], CellClass) ->
    NewCellClass =
        case CellClass of
            "bc" -> "wc";
            "wc" -> "bc"
        end,
    "<td class=" ++ CellClass ++ ">" ++ figure(Figure) ++ "\n" ++
        table_row(Tail, NewCellClass);
table_row(_, _) -> "".

figure(?empty) -> "&nbsp;";
figure(Fig) ->
    "&#" ++ integer_to_list(figure_(Fig)) ++ ";".
figure_(?w_king  ) -> 9812;
figure_(?w_queen ) -> 9813;
figure_(?w_rook  ) -> 9814;
figure_(?w_bishop) -> 9815;
figure_(?w_knight) -> 9816;
figure_(?w_pawn  ) -> 9817;
figure_(?b_king  ) -> 9818;
figure_(?b_queen ) -> 9819;
figure_(?b_rook  ) -> 9820;
figure_(?b_bishop) -> 9821;
figure_(?b_knight) -> 9822;
figure_(?b_pawn  ) -> 9823.

navigation() ->
    Curr = echessd_session:get_val(section),
    "<div class=navig>[&nbsp;" ++
        string:join(
          lists:map(
            fun(Section) ->
                    Caption =
                        case Section of
                            ?SECTION_MAIN -> "Home";
                            ?SECTION_GAMES -> "My games";
                            ?SECTION_USERS -> "Users";
                            _ -> Section
                        end,
                    "<a href='?goto=" ++ Section ++ "'>" ++
                        if Section == Curr ->
                                "<b>" ++ Caption ++ "</b>";
                           true -> Caption
                        end ++ "</a>"
            end,
            [?SECTION_MAIN, ?SECTION_GAMES,
             ?SECTION_USERS, ?SECTION_TEST]),
          "&nbsp;|&nbsp;") ++
        "&nbsp;|&nbsp;<a href='?action=exit'>Logout</a>"
        "&nbsp;]</div>".

login() ->
    header("echessd", []) ++
        h1("echessd login") ++
        "<div class=navig>[&nbsp;<a href='?goto=register'>Register new user</a>&nbsp;]</div>"
        "<form method=post>"
        "<input name=action type=hidden value=login>"
        "Login:    <input name=username type=text><br>"
        "Password: <input name=password type=password><br>"
        "<input type=submit value='Login'>"
        "</form>" ++
        footer([]).

register() ->
    header("echessd", []) ++
        h1("echessd register form") ++
        "<form method=post>"
        "<input name=action type=hidden value=register>"
        "Login:    <input name=regusername type=text><br>"
        "Password: <input name=regpassword1 type=password><br>"
        "Confirm password: <input name=regpassword2 type=password><br>"
        "<input type=submit value='Register'>"
        "</form>" ++
        footer([]).

error(String) ->
    header("echessd", []) ++
        h1("echessd error") ++
        "<div class=error>" ++ String ++ "</div>" ++
        footer([]).

eaccess() -> ?MODULE:error("ACCESS DENIED").

main() ->
    header("echessd", []) ++
        h1("Logged in as \"" ++ get(username) ++ "\"") ++
        navigation() ++
        "" ++
        footer([]).

games() ->
    header("echessd", []) ++
        h1("Logged in as " ++ get(username)) ++
        navigation() ++
        "<a href=''>Start new game</a>" ++
        footer([]).

users() ->
    {ok, Users0} = echessd_user:list(),
    Users = lists:usort(Users0) -- get(username),
    header("echessd", []) ++
        h1("Users list") ++
        navigation() ++
        string:join(
          lists:map(
            fun(User) ->
                    "*&nbsp;<a href='?goto=user&name=" ++ User ++ "'>" ++
                        User ++ "</a>"
            end, Users), "<br>") ++
        "<br><br><a href=''>Start new game</a>" ++
        footer([]).

game(GameID) ->
    header("echessd", []) ++
        h1("Logged in as " ++ get(username)) ++
        navigation() ++
        "" ++
        footer([]).

test_table() ->
    Game0 = echessd_game:new(?GAME_CLASSIC),
    Moves = string:tokens(proplists:get_value("moves", get(query_proplist), ""), ","),
    Game =
        lists:foldl(
          fun(Move, Acc) ->
                  {T, _} = echessd_game:move(Acc, Move),
                  T
          end, Game0, Moves),
    header("echessd", []) ++
        h1("Test table") ++
        navigation() ++
        table(Game) ++
        footer([]).

notyet() ->
    header("echessd", []) ++
        h1("Not implemented yet") ++
        "" ++
        footer([]).

