-module(echessd_html).

-export([login/0,
         register/0,
         error/1,
         eaccess/0,
         home/0,
         game/1,
         users/0,
         user/0,
         newgame/0,
         test_table/0,
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
    header("echessd - Home", []) ++
        h1("Logged in as \"" ++ get(username) ++ "\"") ++
        navigation() ++
        "" ++
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
                    "*&nbsp;<a href='?goto=" ++ ?SECTION_USER ++ "&name=" ++
                        User ++ "'>" ++ User ++ "</a>"
            end, Users), "<br>") ++
        footer([]).

user() ->
    Iam = get(username),
    case proplists:get_value("name", get(query_proplist)) of
        Iam -> home();
        User ->
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
        string:join(
          lists:flatmap(
            fun({created, Time}) ->
                    ["<b>Registered:</b> " ++ echessd_lib:timestamp(Time)];
               (_) ->
                    []
            end, UserInfo), "<br>") ++
        "<br>" ++
        navig_links([{"?goto=" ++ ?SECTION_NEWGAME++ "&user=" ++ User,
                      "Start new game"}]) ++
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
    header("echessd - Game", []) ++
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
    header("echessd - Test table", []) ++
        h1("Test table") ++
        navigation() ++
        table(Game) ++
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
        "<td class=crd_l>" ++ tt(integer_to_list(N)) ++ "\n"
        ++ table_row(Row, N) ++
        "<td class=crd_r>" ++ tt(integer_to_list(N)) ++ "\n",
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

