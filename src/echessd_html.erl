%%% @doc
%%% HTML page generation functions.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 21 Jan 2012
%%% @copyright 2012, Aleksey Morarash

-module(echessd_html).

-export(
   [login/1,
    register/1,
    edituser/1,
    passwd/1,
    eaccess/1,
    home/1,
    game/2,
    draw_confirm/2,
    giveup_confirm/2,
    users/1,
    user/2,
    newgame/2,
    error/2, error/3,
    redirection/2,
    notyet/1
   ]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Make 'login' page.
-spec login(Session :: #session{}) -> HTML :: iolist().
login(Session) ->
    log_reg_page(
      Session, ?SECTION_LOGIN,
      "echessd - " ++ gettext(Session, txt_lgn_title, []),
      [navig_links(
         [{url([{?Q_GOTO, ?SECTION_REG}]),
           gettext(Session, txt_lgn_rnu_link, [])}]),
       case echessd_cfg:get(?CFG_SHOW_ABOUT) of
           true ->
               ["<br>", tag(p, gettext(Session, txt_about, [])), "<br>"];
           _ -> ""
       end,
       "<form method=post>",
       hidden(?Q_GOTO, ?SECTION_LOGIN),
       input(Session, ?Q_USERNAME, txt_lgn_login, ""), "<br>",
       password(Session, ?Q_PASSWORD, txt_lgn_passw), "<br>",
       submit(Session, txt_lgn_ok_button),
       "</form>",
       case echessd_cfg:get(?CFG_SHOW_COPYRIGHTS) of
           true ->
               "<hr>" ++ tag(p, gettext(Session, txt_copyrights, []));
           _ -> ""
       end]).

%% @doc Make 'register new user' page.
-spec register(Session :: #session{}) -> HTML :: iolist().
register(Session) ->
    Optional = [" (", gettext(Session, txt_rnu_optional, []), ")<br>"],
    log_reg_page(
      Session, ?SECTION_REG,
      "echessd - " ++ gettext(Session, txt_rnu_title, []),
      [navig_links([{url([{?Q_GOTO, ?SECTION_LOGIN}]),
                     gettext(Session, txt_rnu_ret_link, [])}]),
       "<form method=post>",
       hidden(?Q_GOTO, ?SECTION_REG),
       input(Session, ?Q_EDIT_USERNAME, txt_rnu_login, ""), "<br>",
       password(Session, ?Q_EDIT_PASSWORD1, txt_rnu_passw), "<br>",
       password(Session, ?Q_EDIT_PASSWORD2, txt_rnu_passw_conf), "<br>",
       input(Session, ?Q_EDIT_FULLNAME, txt_rnu_fullname, ""), Optional,
       select(Session, ?Q_EDIT_TIMEZONE, txt_timezone,
              echessd_lib:local_offset(),
              echessd_lib:administrative_offsets()), "<br>",
       select(Session, ?Q_EDIT_LANGUAGE, txt_rnu_language,
              Session#session.language, echessd_lang:list()), "<br>",
       input(Session, ?Q_EDIT_JID, txt_jid, ""), Optional,
       checkbox(Session, "sil", ?Q_EDIT_SHOW_IN_LIST,
                txt_rnu_show_in_list, true), "<br>",
       submit(Session, txt_rnu_ok_button),
       "</form>"]).

%% @doc Make 'edit user properties' page.
-spec edituser(Session :: #session{}) -> iolist().
edituser(Session) ->
    UserInfo = Session#session.userinfo,
    Title = gettext(Session, txt_edit_profile_title, []),
    [html_page_header(Session, "echessd - " ++ Title, [{h1, Title}]),
     navigation(Session), "<br>",
     navig_links(
       [{url([{?Q_GOTO, ?SECTION_PASSWD_FORM}]),
         gettext(Session, txt_predit_passwd_link, [])}]),
     "<form method=post>",
     hidden(?Q_GOTO, ?SECTION_SAVEUSER),
     input(Session, ?Q_EDIT_FULLNAME, txt_fullname,
           echessd_user:get_value(fullname, UserInfo)), "<br>",
     select(Session, ?Q_EDIT_TIMEZONE, txt_timezone,
            echessd_user:get_value(timezone, UserInfo),
            echessd_lib:administrative_offsets()), "<br>",
     select(Session, ?Q_EDIT_LANGUAGE, txt_predit_lang,
            Session#session.language, echessd_lang:list()), "<br>",
     select(Session, ?Q_EDIT_STYLE, txt_style,
            Session#session.style, echessd_styles:list()), "<br>",
     input(Session, ?Q_EDIT_JID, txt_jid,
           echessd_user:get_value(jid, UserInfo)) ++ " (" ++
         gettext(Session, txt_rnu_optional, []) ++ ")<br>",
     checkbox(Session, "enot", ?Q_EDIT_NOTIFY, txt_notify,
              echessd_user:get_value(notify, UserInfo)), "<br>",
     checkbox(Session, "sil", ?Q_EDIT_SHOW_IN_LIST, txt_rnu_show_in_list,
              echessd_user:get_value(show_in_list, UserInfo)), "<br>",
     checkbox(Session, "sh", ?Q_EDIT_SHOW_HISTORY, txt_rnu_show_history,
              echessd_user:get_value(show_history, UserInfo)), "<br>",
     checkbox(Session, "sc", ?Q_EDIT_SHOW_COMMENT, txt_rnu_show_comment,
              echessd_user:get_value(show_comment, UserInfo)), "<br>",
     checkbox(Session, "autoref", ?Q_EDIT_AUTO_REFRESH, txt_auto_refresh,
              echessd_user:get_value(auto_refresh, UserInfo)), "<br>",
     input(Session, ?Q_EDIT_AUTO_PERIOD, txt_auto_refresh_period,
           echessd_user:get_value(auto_refresh_period, UserInfo)), "<br>",
     submit(Session, txt_predit_save_button),
     "</form><br>",
     html_page_footer()].

%% @doc Make 'change user password' page.
-spec passwd(Session :: #session{}) -> HTML :: iolist().
passwd(Session) ->
    Title = gettext(Session, txt_passwd_title, []),
    [html_page_header(Session, "echessd - " ++ Title, [{h1, Title}]),
     navigation(Session),
     "<br><form method=post>",
     hidden(?Q_GOTO, ?SECTION_PASSWD),
     gettext(Session, txt_passwd_passw, []) ++ ": " ++
         "<input name=editpassword0 type=password><br>",
     gettext(Session, txt_passwd_passw_new, []) ++ ": " ++
         "<input name=editpassword1 type=password><br>",
     gettext(Session, txt_passwd_passw_new_confirm, []), ": " ++
         "<input name=editpassword2 type=password><br>",
     submit(Session, txt_passwd_save_button),
     "</form><br>",
     html_page_footer()].

%% @doc Make 'home' page.
-spec home(Session :: #session{}) -> HTML :: iolist().
home(Session) ->
    Username = Session#session.username,
    UserInfo = Session#session.userinfo,
    Title = gettext(Session, txt_home, []),
    [html_page_header(Session, "echessd - " ++ Title,
                      [{h1, Title ++ ": " ++ Username}]),
     navigation(Session),
     navig_links([{url([{?Q_GOTO, ?SECTION_EDITUSER}]),
                   gettext(Session, txt_edit_profile_title, [])}]), "<br>",
     user_info(Session, Username, UserInfo), "<br>",
     user_games(Session, Username, UserInfo, true),
     case echessd_user:get_value(auto_refresh, UserInfo) of
         true ->
             Period =
                 integer_to_list(
                   echessd_user:get_value(
                     auto_refresh_period, UserInfo) * 1000),
             tag(
               script, [],
               "setTimeout(\"document.location.href='/'\"," ++
                   Period ++ ")");
         _ -> ""
     end,
     html_page_footer()].

%% @doc Make 'registered users list' page.
-spec users(Session :: #session{}) -> HTML :: iolist().
users(Session) ->
    {ok, Users0} = echessd_user:list(),
    Users = lists:usort(Users0) -- [Session#session.username],
    Title = gettext(Session, txt_users, []),
    [html_page_header(Session, "echessd - " ++ Title, [{h1, Title}]),
     navigation(Session), "<br>",
     string:join([["*&nbsp;", userlink(U)] || U <- Users], "<br>"),
     html_page_footer()].

%% @doc Make 'user details' page.
-spec user(Session :: #session{},
           Query :: echessd_query_parser:http_query()) ->
                  HTML :: iolist().
user(Session, Query) ->
    case proplists:get_value(?Q_NAME, Query) of
        MyName when MyName == Session#session.username ->
            home(Session);
        Opponent ->
            case echessd_user:getprops(Opponent) of
                {ok, OppInfo} ->
                    opponent(Session, Opponent, OppInfo);
                {error, Reason} ->
                    ?MODULE:error(
                       gettext(Session, txt_user_fetch_error, []) ++ ":~n~p",
                       [Opponent, Reason])
            end
    end.

%% @private
%% @doc user/2 helper fun.
-spec opponent(Session :: #session{},
               Opponent :: echessd_user:echessd_user(),
               OppInfo :: echessd_user:user_info()) ->
                  HTML :: iolist().
opponent(Session, Opponent, OppInfo) ->
    Title = gettext(Session, txt_user, []) ++ " '" ++ Opponent ++ "'",
    [html_page_header(Session, "echessd - " ++ Title, [{h1, Title}]),
     navigation(Session), "<br>",
     user_info(Session, Opponent, OppInfo), "<br>",
     user_games(Session, Opponent, OppInfo, false), "<br>",
     newgame_link(Session, Opponent),
     html_page_footer()].

%% @doc Make 'create new game' page.
-spec newgame(Session :: #session{},
              Query :: echessd_query_parser:http_query()) ->
                     HTML :: iolist().
newgame(Session, Query) ->
    Opponent = proplists:get_value(?Q_USER, Query),
    Iam = Session#session.username,
    H2Title =
        if Iam == Opponent ->
                gettext(Session, txt_ng_title_h2_test, [Iam, Iam]);
           true ->
                gettext(Session, txt_ng_title_h2_normal, [Iam, Opponent])
        end,
    Title = gettext(Session, txt_ng_title, []),
    [html_page_header(Session, "echessd - " ++ Title, [{h1, Title}]),
     navigation(Session),
     h2(H2Title),
     "<form method=post>",
     hidden(?Q_GOTO, ?SECTION_NEWGAME),
     hidden(?Q_OPPONENT, Opponent),
     hidden(?Q_GAMETYPE, ?GAME_CLASSIC),
     if Iam == Opponent ->
             hidden(?Q_COLOR, ?white);
        true ->
             select(Session, ?Q_COLOR, txt_ng_color, undefined,
                    [{random, gettext(Session, txt_ng_color_random, [])},
                     {?white, gettext(Session, txt_ng_color_white, [])},
                     {?black, gettext(Session, txt_ng_color_black, [])}])
     end,
     checkbox(Session, "prv", ?Q_PRIVATE, txt_ng_private, false), "<br>",
     submit(Session, txt_ng_ok_button),
     "</form>",
     html_page_footer()].

%% @doc Make 'game' page.
-spec game(Session :: #session{},
           Query :: echessd_query_parser:http_query()) ->
                  HTML :: iolist().
game(Session, Query) ->
    case fetch_game(Session, GameID = proplists:get_value(?Q_GAME, Query)) of
        {ok, GameInfo} ->
            game(Session, Query, GameID, GameInfo);
        ErrorContent ->
            ErrorContent
    end.

%% @private
%% @doc game/2 helper fun.
-spec game(Session :: #session{},
           Query :: echessd_query_parser:http_query(),
           GameID :: echessd_game:echessd_game_id(),
           GameInfo :: echessd_game:echessd_game_info()) ->
                  HTML :: iolist().
game(Session, Query, GameID, GameInfo) ->
    Step = proplists:get_value(?Q_STEP, Query),
    Iam = Session#session.username,
    GameType = proplists:get_value(type, GameInfo),
    FullHistory = proplists:get_value(moves, GameInfo, []),
    FullHistoryLen = length(FullHistory),
    History =
        case Step of
            last -> FullHistory;
            _ -> lists:sublist(FullHistory, Step)
        end,
    HistoryLen = length(History),
    IsLast = HistoryLen == FullHistoryLen,
    {Board, Captures} =
        echessd_game:from_scratch(GameType, History),
    Players =
        [I || {users, L} <- GameInfo, {_, C} = I <- L,
              lists:member(C, [?black, ?white])],
    Users = lists:usort([N || {N, _} <- Players]),
    Opponent =
        case Users -- [Iam] of
            [Opponent0 | _] -> Opponent0;
            _ -> hd(Users)
        end,
    IsMyGame = lists:member(Iam, Users),
    MyColors = [C || {N, C} <- Players, N == Iam],
    TurnColor = echessd_game:turn_color(GameInfo),
    TurnUser = hd([N || {N, C} <- Players, C == TurnColor]),
    WaitColor = hd([?black, ?white] -- [TurnColor]),
    WaitUser = hd([N || {N, C} <- Players, C == WaitColor]),
    IsRotated =
        if TurnUser == WaitUser andalso
           WaitUser == Iam ->
                TurnColor == ?black;
           true ->
                IsMyGame andalso
                    lists:member(?black, MyColors)
        end,
    {LastPly, Comment} =
        case lists:reverse(History) of
            [{LastPly0, Meta} | _] ->
                {LastPly0, proplists:get_value(comment, Meta, "")};
            [LastPly0 | _] -> {LastPly0, ""};
            _ -> {undefined, ""}
        end,
    GameStatus = proplists:get_value(status, GameInfo, none),
    Winner = proplists:get_value(winner, GameInfo),
    IsMyTurn = TurnUser == Iam andalso GameStatus == none,
    Title =
        if IsMyTurn ->
                "echessd: " ++ gettext(Session, txt_your_move, []);
           true ->
                "echessd: " ++ gettext(Session, txt_game, []) ++ " #" ++
                    integer_to_list(GameID)
        end,
    {GroupedPossibles, ActiveCells} =
        if IsMyTurn andalso IsLast ->
                GroupedPossibles0 =
                    group_possibles(
                      echessd_game:possibles(GameType, History), []),
                {GroupedPossibles0,
                 lists:usort(
                   lists:append(
                     [[I1 | L] || {I1, L} <- GroupedPossibles0]))};
           true -> {[], []}
        end,
    UserInfo =
        if is_list(Session#session.userinfo) ->
                Session#session.userinfo;
           true ->
                []
        end,
    ShowHistory = echessd_user:get_value(show_history, UserInfo),
    ShowComment = echessd_user:get_value(show_comment, UserInfo),
    AutoRefresh =
        (not IsMyTurn) andalso
        echessd_user:get_value(auto_refresh, UserInfo),
    AutoRefreshPeriod = echessd_user:get_value(auto_refresh_period, UserInfo),
    ChessTable =
        chess_table(
          GameID, HistoryLen, IsLast, GameType, Board,
          IsRotated, ActiveCells, LastPly) ++
        case Comment of
            [_ | _] when ShowComment ->
                tag(p, gettext(Session, txt_comment, []) ++ ": " ++ Comment);
            _ -> ""
        end ++
        if IsMyTurn andalso IsLast ->
                Possibles =
                    echessd_game:possibles(GameType, History),
                echessd_log:debug("Possibles: ~9999p", [Possibles]),
                tag(script, ["src='/res/echessd.js'"], "") ++
                    tag(script,
                        js_init(GroupedPossibles,
                                ActiveCells, LastPly)) ++
                    "<form method=post>" ++
                    gettext(Session, txt_move_caption, []) ++ ":&nbsp;"
                    "<input name=action type=hidden value=move>"
                    "<input name=game type=hidden value=" ++
                    integer_to_list(GameID) ++ ">"
                    "<input name=move type=text size=5 id=edmv>"
                    "<input type=submit class=btn value='" ++
                    gettext(Session, txt_move_ok_button, []) ++ "'>"
                    "<input type=reset class=btn value='" ++
                    gettext(Session, txt_move_reset_button, []) ++ "' "
                    "onclick='clr();'><br>" ++
                    gettext(Session, txt_move_comment_caption, []) ++ ":&nbsp;"
                    "<input type=text name=comment>"
                    "</form><br>";
            true -> ""
        end ++
        captures(Captures),
    [html_page_header(Session, Title, []),
     game_navigation(Session, GameID, IsMyGame andalso GameStatus == none),
     case GameStatus of
         checkmate ->
             h2(gettext(Session, txt_gt_over_checkmate, [userlink(Winner)]));
         give_up ->
             h2(gettext(Session, txt_gt_over_giveup, [userlink(Winner)]));
         {draw, stalemate} ->
             h2(gettext(Session, txt_gt_over_stalemate, []));
         {draw, agreement} ->
             h2(gettext(Session, txt_gt_over_agreement, []));
         {draw, DrawType} ->
             h2(gettext(Session, txt_gt_over_draw, [DrawType]));
         _ ->
             case proplists:get_value(
                    draw_request_from, GameInfo) of
                 Iam when IsMyGame ->
                     warning(Session, txt_gt_youre_drawing, []);
                 Opponent when IsMyGame ->
                     warning(Session, txt_gt_opponent_drawing,
                             [userlink(Opponent)]);
                 _ -> ""
             end
     end,
     if ShowHistory ->
             tag(table, ["cellpadding=0", "cellspacing=0", "border=0"],
                 tr(
                   [tag(td, ["valign=top"], ChessTable),
                    tag(td, ["valign=top"],
                        game_history(Step, GameID, FullHistory))]));
        true ->
             ChessTable
     end,
     if AutoRefresh ->
             Period = integer_to_list(AutoRefreshPeriod * 1000),
             tag(
               script,
               "setTimeout(\"document.location.href='" ++
                   url([{?Q_GOTO, ?SECTION_GAME}, {?Q_GAME, GameID},
                        {?Q_STEP, Step}]) ++ "'\"," ++ Period ++ ")");
        true -> ""
     end,
     html_page_footer()].

%% @doc Make 'draw confirmation' page.
-spec draw_confirm(Session :: #session{},
                   Query :: echessd_query_parser:http_query()) ->
                          HTML :: iolist().
draw_confirm(Session, Query) ->
    GameID = proplists:get_value(?Q_GAME, Query),
    Title = gettext(Session, txt_draw_confirm_title, []),
    [html_page_header(Session, "echessd - " ++ Title, [{h1, Title}]),
     warning(Session, txt_draw_confirm_text, []),
     "<form method=post>",
     "<input type=hidden name=action value=" ++ ?SECTION_DRAW ++ ">",
     "<input type=hidden name=game value=" ++
         integer_to_list(GameID) ++ ">",
     "<input type=submit class=btn value='" ++
         gettext(Session, txt_draw_button, []) ++ "'>",
     "</form>",
     navig_links([{"javascript: history.back();",
                   gettext(Session, txt_ouch_back_link, [])}]),
     html_page_footer()].

%% @doc Make 'giving up confirmation' page.
-spec giveup_confirm(Session :: #session{},
                     Query :: echessd_query_parser:http_query()) ->
                            HTML :: iolist().
giveup_confirm(Session, Query) ->
    GameID = proplists:get_value(?Q_GAME, Query),
    Iam = Session#session.username,
    {ok, GameInfo} = echessd_game:getprops(GameID),
    Players =
        [N || {users, L} <- GameInfo,
              {N, C} <- L, lists:member(C, [?white, ?black])],
    [Opponent | _] = Players -- [Iam],
    Title = gettext(Session, txt_giveup_confirm_title, []),
    [html_page_header(Session, "echessd - " ++ Title, [{h1, Title}]),
     warning(Session, txt_giveup_confirm_text, [userlink(Opponent)]),
     "<form method=post>",
     "<input type=hidden name=action value=" ++
         atom_to_list(?SECTION_GIVEUP) ++ ">",
     "<input type=hidden name=game value=" ++
         integer_to_list(GameID) ++ ">",
     "<input type=submit class=btn value='" ++
         gettext(Session, txt_giveup_button, []) ++ "'>",
     "</form>",
     navig_links([{"javascript: history.back();",
                   gettext(Session, txt_ouch_back_link, [])}]),
     html_page_footer()].

%% @doc Make 'under construction' page.
-spec notyet(Session :: #session{}) -> iolist().
notyet(Session) ->
    Title = gettext(Session, txt_not_implemented_title, []),
    [html_page_header(Session, "echessd - " ++ Title, [{h1, Title}]),
     h2(gettext(Session, txt_not_implemented_text, [])),
     navig_links([{"javascript: history.back();",
                   gettext(Session, txt_ouch_back_link, [])}]),
     html_page_footer()].

%% @doc Make 'error' page.
-spec error(Session :: #session{}, Message :: iolist()) -> HTML :: iolist().
error(Session, Message) ->
    Title = gettext(Session, txt_error_page_title, []),
    [html_page_header(Session, "echessd - " ++ Title, [{h1, Title}]),
     tag("div", ["class=error"], pre(Message)), "<br>",
     navig_links([{"javascript: history.back();",
                   gettext(Session, txt_back_link, [])}]),
     html_page_footer()].

%% @doc Make 'error' page.
-spec error(Session :: #session{},
            Format :: string(), Args :: list()) -> HTML :: iolist().
error(Session, Format, Args) ->
    ?MODULE:error(Session, io_lib:format(Format, Args)).

%% @doc Make 'access denied' page.
-spec eaccess(Session :: #session{}) -> HTML :: iolist().
eaccess(Session) ->
    ?MODULE:error(gettext(Session, txt_access_denied, [])).

%% @doc Make 303-redirection page.
-spec redirection(Session :: #session{},
                  URL :: nonempty_string()) -> HTML :: iolist().
redirection(Session, URL) ->
    Title = gettext(Session, txt_redirection, []),
    [html_page_header(Session, "echessd - " ++ Title, [{h1, Title}]),
     tag(p, gettext(Session, txt_redirection_description, [])),
     a(URL, URL),
     html_page_footer()].

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc
-spec js_init(Grouped :: list(), ActiveCells :: list(),
              LastPly :: nonempty_string()) ->
                     HTML :: iolist().
js_init(Grouped, ActiveCells, LastPly) ->
    ["var ACs = [",
     string:join(["'" ++ C ++ "'" || C <- ActiveCells], ","),
     "];\nvar I1s = [",
     string:join(["'" ++ I1 ++ "'" || {I1, _} <- Grouped], ","),
     "];\nvar I2s = [",
     string:join(
       [[$[, string:join([[$', I2, $'] || I2 <- L], ","), $]] ||
           {_, L} <- Grouped], ","),
     "];\nvar LPs = [",
     case LastPly of
         [A, B, C, D] ->
             [$', [A, B], "','", [C, D], $'];
         _ -> ""
     end, "];\n"].

%% @doc
-spec group_possibles(Plies :: list(), Acc :: list()) -> FinalAcc :: list().
group_possibles([], Result) ->
    Result;
group_possibles([[A, B, C, D] | Tail], [{[A, B], List} | ResTail]) ->
    group_possibles(Tail, [{[A, B], [[C, D] | List]} | ResTail]);
group_possibles([[A, B, C, D] | Tail], Result) ->
    group_possibles(Tail, [{[A, B], [[C, D]]} | Result]).

%% @doc
-spec log_reg_page(Session :: #session{},
                   Section :: echessd_query_parser:section(),
                   Title :: string(),
                   HtmlPagePayload :: iolist()) -> HtmlPage :: iolist().
log_reg_page(Session, Section, Title, Content) ->
    html_page_header(Session, Title, []) ++
        navig_links(
          lists:map(
            fun(LangID) ->
                    {echessd_query_parser:encode(
                       [{?Q_GOTO, Section}, {?Q_LANG, LangID}]), LangID}
            end, [LangID || {LangID, _} <- echessd_lang:list()])) ++
        h1(Title) ++ Content ++
        html_page_footer().

%% @doc
-spec fetch_game(Session :: #session{},
                 GameID :: echessd_game:echessd_game_id()) ->
                        {ok, GameInfo :: echessd_game:echessd_gameinfo()} |
                        (ErrorPagePayload :: iolist()).
fetch_game(Session, GameID) ->
    case echessd_game:getprops(GameID) of
        {ok, GameInfo} ->
            case proplists:get_value(private, GameInfo) of
                true ->
                    case is_my_game(Session, GameInfo) of
                        true ->
                            {ok, GameInfo};
                        _ ->
                            ?MODULE:error(
                               gettext(Session, txt_game_fetch_error, []) ++
                                   ":~n~p", [GameID, {no_such_game, GameID}])
                    end;
                _ ->
                    case proplists:get_value(
                           acknowledged, GameInfo) of
                        true ->
                            {ok, GameInfo};
                        _ ->
                            ?MODULE:error(
                               gettext(
                                 Session, txt_game_not_confirmed_error,
                                 [GameID]))
                    end
            end;
        {error, Reason} ->
            ?MODULE:error(
               gettext(Session, txt_game_fetch_error, []) ++ ":~n~p",
               [GameID, Reason])
    end.

%% @doc
-spec is_my_game(Session :: #session{},
                 GameInfo :: echessd_game:echessd_gameinfo()) ->
                        boolean().
is_my_game(Session, GameInfo) ->
    Iam = Session#session.username,
    Users = proplists:get_value(users, GameInfo, []),
    case [N || {N, C} <- Users, N == Iam,
               lists:member(C, [?white, ?black])] of
        [_ | _] ->
            true;
        _ ->
            false
    end.

%% @doc Make 'user info' table.
-spec user_info(Session :: #session{},
                Username :: echessd_user:echessd_user(),
                UserInfo :: echessd_user:echessd_userinfo()) ->
                       HTML :: iolist().
user_info(Session, Username, UserInfo) ->
    tag("table", ["cellpadding=0", "cellspacing=0"],
        tr(
          string:join(
            [tr(td(b(K ++ ":&nbsp;")) ++ td(V)) ||
                {K, V} <- user_info_cells(Session, Username, UserInfo)]
            , "\n"))).

%% @doc Return the cells for the 'user info' table.
-spec user_info_cells(Session :: #session{},
                      Username :: echessd_user:echessd_user(),
                      UserInfo :: echessd_user:echessd_userinfo()) ->
                             [{CellCaption :: iolist(),
                               CellValue :: iolist()}].
user_info_cells(Session, Username, UserInfo) ->
    LangInfo = echessd_user:lang_info(UserInfo),
    lists:flatmap(
      fun(login) -> [{gettext(Session, txt_login, []), Username}];
         (fullname = Key) ->
              [{gettext(Session, txt_fullname, []),
                case echessd_user:get_value(Key, UserInfo) of
                    [_ | _] = Value ->
                        echessd_lib:escape_html_entities(Value);
                    _ -> gettext(Session, txt_not_sure, [])
                end}];
         (created = Key) ->
              [{gettext(Session, txt_registered, []),
                case echessd_user:get_value(Key, UserInfo) of
                    Value when ?is_now(Value) ->
                        echessd_lib:timestamp(
                          Value, get(timezone));
                    _ -> gettext(Session, txt_unknown, [])
                end}];
         (timezone = Key) ->
              [{gettext(Session, txt_timezone, []),
                echessd_lib:time_offset_to_list(
                  echessd_user:get_value(Key, UserInfo))}];
         (language) ->
              {_LangAbbr, LangName} = LangInfo,
              [{gettext(Session, txt_language, []), LangName}];
         (_) -> []
      end, [login, fullname, created, timezone, language]).

%% @doc
-spec user_games(Session :: #session{},
                 Username :: echessd_user:echessd_user(),
                 UserInfo :: echessd_user:echessd_userinfo(),
                 ShowNotAcknowledged :: boolean()) ->
                        HTML :: iolist().
user_games(Session, Username, UserInfo, ShowNotAcknowledged) ->
    %% fetch all user games info
    UserGames =
        lists:flatmap(
          fun(GameID) ->
                  case echessd_game:getprops(GameID) of
                      {ok, GameInfo} ->
                          Visible =
                              not (proplists:get_value(private, GameInfo) == true)
                              orelse is_my_game(Session, GameInfo),
                          if Visible -> [{GameID, GameInfo}];
                             true -> []
                          end;
                      {error, Reason} ->
                          echessd_log:err(
                            "Failed to fetch game #~w props: "
                            "~9999p (reference from ~9999p user)",
                            [GameID, Reason, Username]),
                          []
                  end
          end, echessd_user:get_value(games, UserInfo)),
    %% split not acknowledged
    {Confirmed, NotConfirmed} =
        lists:partition(
          fun({_GameID, GameInfo}) ->
                  proplists:get_value(acknowledged, GameInfo)
          end, UserGames),
    %% split ended
    {NotEnded, Ended} =
        lists:partition(
          fun({_GameID, GameInfo}) ->
                  proplists:get_value(status, GameInfo) == none
          end, Confirmed),
    case NotEnded of
        [_ | _] ->
            h2(gettext(Session, txt_user_games, []) ++ ":") ++
                string:join(
                  [user_game_(Session, Username, I, L) ||
                      {I, L} <- NotEnded], "<br>") ++
                "<br>";
        _ -> ""
    end ++
        case NotConfirmed of
            [_ | _] when ShowNotAcknowledged ->
                h2(gettext(Session, txt_unconf_games, []) ++ ":") ++
                    string:join(
                      [user_unconfirmed_game_(Session, Username, I, L) ||
                          {I, L} <- NotConfirmed], "<br>") ++
                    "<br>";
            _ -> ""
        end ++
        case Ended of
            [_ | _] ->
                h2(gettext(Session, txt_user_ended_games, []) ++ ":") ++
                    string:join(
                      [user_game_(Session, Username, I, L) ||
                          {I, L} <- Ended], "<br>") ++
                    "<br>";
            _ -> ""
        end.

%% @doc
-spec user_game_(Session :: #session{},
                 Owner :: echessd_user:echessd_user(),
                 GameID :: echessd_game:echessd_game_id(),
                 GameInfo :: echessd_game:echessd_gameinfo()) ->
                        HTML :: iolist().
user_game_(Session, Owner, GameID, GameInfo) ->
    GamePlayers =
        [{N, C} || {N, C} <- proplists:get_value(users, GameInfo, []),
                   lists:member(C, [?white, ?black])],
    UniquePlayerNames = lists:usort([N || {N, _} <- GamePlayers]),
    IsTest = length(UniquePlayerNames) == 1,
    "* " ++ gamelink(GameID) ++
        if IsTest ->
                " " ++ gettext(Session, txt_test_game, []);
           true ->
                Color = proplists:get_value(Owner, GamePlayers),
                Opponent = hd(UniquePlayerNames -- [Owner]),
                OpponentColor = proplists:get_value(Opponent, GamePlayers),
                " " ++ chessman({Color, ?king}) ++ " " ++
                    gettext(Session, txt_vs, []) ++
                    " " ++ userlink(Opponent) ++ " " ++
                    chessman({OpponentColor, ?king})
        end ++
        case proplists:get_value(status, GameInfo) of
            none ->
                case get(username) == echessd_game:who_must_turn(GameInfo) of
                    true when not IsTest ->
                        " !!!";
                    _ ->
                        ""
                end;
            checkmate when IsTest ->
                " - " ++ gettext(Session, txt_checkmate, []);
            checkmate ->
                case proplists:get_value(winner, GameInfo) of
                    Owner ->
                        " - " ++ gettext(Session, txt_win, []);
                    _ ->
                        " - " ++ gettext(Session, txt_loose, [])
                end;
            give_up when IsTest ->
                " - " ++ gettext(Session, txt_gived_up, []);
            give_up ->
                case proplists:get_value(winner, GameInfo) of
                    Owner ->
                        " - " ++ gettext(Session, txt_win_giveup, []);
                    _ ->
                        " - " ++ gettext(Session, txt_loose_giveup, [])
                end;
            {draw, _} ->
                " - " ++ gettext(Session, txt_draw, [])
        end.

%% @doc
-spec user_unconfirmed_game_(Session :: #session{},
                             Owner :: echessd_user:echessd_user(),
                             GameID :: echessd_game:echessd_game_id(),
                             GameInfo :: echessd_game:echessd_gameinfo()) ->
                                    HTML :: iolist().
user_unconfirmed_game_(Session, Owner, GameID, GameInfo) ->
    GamePlayers =
        [{N, C} || {N, C} <- proplists:get_value(users, GameInfo, []),
                   lists:member(C, [?white, ?black])],
    UniquePlayerNames = lists:usort([N || {N, _} <- GamePlayers]),
    "* #" ++ integer_to_list(GameID) ++ " " ++
        case proplists:get_value(creator, GameInfo) of
            Owner ->
                Opponent = hd(UniquePlayerNames -- [Owner]),
                OpponentColor = proplists:get_value(Opponent, GamePlayers),
                gettext(Session, txt_waiting_for,
                        [userlink(Opponent) ++ " " ++
                             chessman({OpponentColor, ?king})]);
            Opponent ->
                OpponentColor = proplists:get_value(Opponent, GamePlayers),
                AckURL = url([{?Q_GOTO, ?SECTION_ACKGAME}, {?Q_GAME, GameID}]),
                gettext(Session, txt_waiting_for_you,
                        [userlink(Opponent) ++ " " ++
                             chessman({OpponentColor, ?king})]) ++ " " ++
                    tag(a, ["href='" ++ AckURL ++ "'"],
                        gettext(Session, txt_game_confirm, []))
        end ++ " " ++
        tag(a, ["href='" ++
                    url([{?Q_GOTO, ?SECTION_DENYGAME}, {?Q_GAME, GameID}]) ++
                    "'"],
            gettext(Session, txt_game_deny, [])).

%% @doc
-spec html_page_header(Session :: #session{},
                       Title :: string(),
                       Options :: list()) ->
                              HtmlPageHeader :: iolist().
html_page_header(Session, Title, Options) ->
    UserStyle = Session#session.style,
    StylesFilename =
        case [F || {N, _T, F} <- echessd_styles:list(), N == UserStyle] of
            [Filename0 | _] -> Filename0;
            _ ->
                {_N, _T, Filename0} = echessd_lib:default_style(),
                Filename0
        end,
    "<html>\n\n"
        "<head>\n"
        "<meta http-equiv='Content-Type' content='text/html; charset=utf-8'>\n"
        "<meta http-equiv='Content-Style-Type' content='text/css'>\n"
        "<meta http-equiv='Content-Script-Type' content='text/javascript'>\n"
        "<title>" ++ Title ++ "</title>\n"
        "<link rel='stylesheet' href='/res/" ++ StylesFilename ++ "'>\n"
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
                    pre(gettext(Session, txt_error, []) ++ ": " ++
                            format_error(Session, Error)))
        end.

%% @doc
-spec html_page_footer() -> HtmlPageFooter :: iolist().
html_page_footer() ->
    "\n\n</body>\n</html>\n".

%% @doc
-spec h1(String :: string()) -> HTML :: iolist().
h1(String) ->
    tag(h1, String).

%% @doc
-spec h2(String :: string()) -> HTML :: iolist().
h2(String) ->
    tag(h2, String).

%% @doc
-spec b(String :: string()) -> HTML :: iolist().
b(String) ->
    tag(b, String).

%% @doc
-spec tt(String :: string()) -> HTML :: iolist().
tt(String) ->
    tag(tt, String).

%% @doc
-spec td(String :: string()) -> HTML :: iolist().
td(String) ->
    tag(td, String).

%% @doc
-spec tr(String :: string()) -> HTML :: iolist().
tr(String) ->
    tag(tr, String).

%% @doc
-spec pre(String :: string()) -> HTML :: iolist().
pre(String) ->
    tag(pre, String).

%% @doc
-spec a(URL :: string(), Caption :: string()) -> HTML :: iolist().
a(URL, Caption) ->
    tag(a, ["href='" ++ URL ++ "'"], Caption).

%% @equiv tag(Tag, [], Value)
%% @doc
-spec tag(Tag :: atom() | nonempty_string(),
          Value :: iolist()) -> HTML :: iolist().
tag(Tag, Value) ->
    tag(Tag, [], Value).

%% @doc
-spec tag(Tag :: atom() | nonempty_string(),
          Attrs :: [string()],
          Value :: iolist()) -> HTML :: iolist().
tag(Tag, Attrs, Value) when is_atom(Tag) ->
    tag(atom_to_list(Tag), Attrs, Value);
tag([_ | _] = Tag, Attrs, Value) ->
    [$<, Tag, [[$\s | V] || V <- Attrs], $>, Value, "</", Tag, $>].

%% @doc
-spec checkbox(Session :: #session{},
               ID :: nonempty_string(),
               Name :: echessd_query_parser:http_query_key(),
               CaptionTextID :: atom(),
               IsChecked :: boolean()) ->
                      HTML :: iolist().
checkbox(Session, ID, Name, CaptionTextID, IsChecked) ->
    [io_lib:format(
       "<label for=~s><input type=checkbox id=~s name=~w", [ID, ID, Name]),
     proplists:get_value(IsChecked, [{false, ""}], " checked"), ">&nbsp;",
     gettext(Session, CaptionTextID, []), "</label>"].

%% @doc
-spec input(Session :: #session{},
            Name :: echessd_query_parser:http_query_key(),
            CaptionTextID :: atom(),
            Value :: any()) ->
                   HTML :: iolist().
input(Session, Name, CaptionTextID, Value) ->
    EncodedValue =
        echessd_lib:escape_html_entities(
          echessd_query_parser:encode(Name, Value)),
    [gettext(Session, CaptionTextID, []), ": ",
     io_lib:format("<input name=~w type=text value='~s'>",
                   [Name, EncodedValue])].

%% @doc
-spec password(Session :: #session{},
               Name :: echessd_query_parser:http_query_key(),
               CaptionTextID :: atom()) -> HTML :: iolist().
password(Session, Name, CaptionTextID) ->
    [gettext(Session, CaptionTextID, []), ": ",
     io_lib:format("<input name=~w type=password>", [Name])].

%% @doc
-spec submit(Session :: #session{}, CaptionTextID :: atom()) ->
                    HTML :: iolist().
submit(Session, CaptionTextID) ->
    ["<input type=submit class=btn value='",
     echessd_lib:escape_html_entities(
       gettext(Session, CaptionTextID, [])), "'>"].

%% @doc
-spec hidden(Name :: echessd_query_parser:http_query_key(),
             Value :: any()) -> HTML :: iolist().
hidden(Name, Value) ->
    EncodedValue =
        echessd_lib:escape_html_entities(
          echessd_query_parser:encode(Name, Value)),
    io_lib:format("<input name=~w type=hidden value='~s'>",
                  [Name, EncodedValue]).

%% @doc
-spec select(Session :: #session{},
             Name :: echessd_query_parser:http_query_key(),
             CaptionTextID :: atom(),
             Value :: any(),
             Items :: [{ItemValue :: any(), Caption :: string()}]) ->
                    HTML :: iolist().
select(Session, Name, CaptionTextID, Value, Items) ->
    EncodedValue =
        echessd_lib:escape_html_entities(
          echessd_query_parser:encode(Name, Value)),
    [gettext(Session, CaptionTextID, []),
     ": <select name='", atom_to_list(Name), "'>",
     lists:map(
       fun({ItemValue, Caption}) ->
               EncodedItemValue =
                   echessd_lib:escape_html_entities(
                     echessd_query_parser:encode(Name, ItemValue)),
               select_option(EncodedItemValue, Caption,
                             EncodedItemValue == EncodedValue);
          (ItemValue) ->
               EncodedItemValue =
                   echessd_lib:escape_html_entities(
                     echessd_query_parser:encode(Name, ItemValue)),
               select_option(EncodedItemValue, EncodedItemValue,
                             EncodedItemValue == EncodedValue)
       end, Items),
     "</select>"].

%% @doc
-spec select_option(Value :: iolist(),
                    Caption :: iolist(),
                    IsSelected :: boolean()) ->
                           HTML :: iolist().
select_option(Value, Caption, IsSelected) ->
    ["<option value='", Value, "'",
     proplists:get_value(IsSelected, [{true, " selected"}], ""),
     ">", Caption, "</option>"].

%% @doc
-spec userlink(Username :: echessd_user:echessd_user()) ->
                      HTML :: iolist().
userlink(Username) ->
    a(url([{?Q_GOTO, ?SECTION_USER}, {?Q_NAME, Username}]), Username).

%% @doc
-spec gamelink(GameID :: echessd_game:echessd_game_id()) ->
                      HTML :: iolist().
gamelink(GameID) ->
    a(url([{?Q_GOTO, ?SECTION_GAME}, {?Q_GAME, GameID}]),
      ["#", echessd_query_parser:encode(?Q_GAME, GameID)]).

%% @doc
-spec newgame_link(Session :: #session{},
                   WithUsername :: echessd_user:echessd_user()) ->
                          HTML :: iolist().
newgame_link(Session, WithUsername) ->
    navig_links(
      [{url([{?Q_GOTO, ?SECTION_GAME}, {?Q_USER, WithUsername}]),
        gettext(Session, txt_new_game_link, [])}]).

%% @doc
-spec chess_table(GameID :: echessd_game:echessd_game_id(),
                  Step :: non_neg_integer() | last,
                  IsLast :: boolean(),
                  GameType :: echessd_query_parser:gametype(),
                  Board :: any(),
                  IsRotated :: boolean(),
                  ActiveCells :: any(),
                  LastPly :: any()) ->
                         HTML :: iolist().
chess_table(GameID, Step, IsLast, _GameType, Board,
            IsRotated, ActiveCells, LastPly) ->
    {ColStep, RowStep} =
        if IsRotated -> {-1, 1};
           true -> {1, -1}
        end,
    Color =
        fun(C, R) when (C + R) rem 2 == 0 -> "bc";
           (_, _) -> "wc"
        end,
    ExtraAction =
        fun(Crd, true) ->
                ["onclick=\"mv('" ++ Crd ++ "');\""];
           (_, _) -> []
        end,
    tag(
      "table", ["cellpadding=0", "cellspacing=0"],
      tr(td("") ++ [tag("td", ["class=crd_t"], tt([$a + C - 1])) ||
                       C <- cseq(ColStep)] ++ td("")) ++
          lists:map(
            fun(R) ->
                    tr(tag("td", ["class=crd_l"], tt([$1 + R - 1])) ++
                           lists:map(
                             fun(C) ->
                                     Crd = [$a + C - 1, $1 + R - 1],
                                     IsActive =
                                         lists:member(
                                           Crd, ActiveCells),
                                     CellClass =
                                         proplists:get_value(
                                           inply(LastPly, Crd),
                                           [{true, "lastply"}], "cell"),
                                     tag("td",
                                         ["class=" ++ Color(C, R)] ++
                                             ExtraAction(Crd, IsActive),
                                         tag("div",
                                             ["class=" ++ CellClass] ++
                                                 if IsActive -> ["id=" ++ Crd];
                                                    true -> []
                                                 end,
                                             chessman(cell(Board, C, R))))
                             end, cseq(ColStep)) ++
                           tag("td", ["class=crd_r"], tt([$1 + R - 1])))
            end, cseq(RowStep)) ++
          tr(td("") ++ [tag("td", ["class=crd_b"], tt([$a + C - 1])) ||
                           C <- cseq(ColStep)] ++ td("")) ++
          tr(
            td("") ++
                tag("td", ["colspan=8"], hist_buttons(GameID, Step, IsLast)) ++
                td(""))).

%% @doc
-spec hist_buttons(GameID :: echessd_game:echessd_game_id(),
                   Step :: non_neg_integer() | last,
                   IsLast :: boolean()) ->
                          HTML :: iolist().
hist_buttons(GameID, Step, IsLast) ->
    Hiddens =
        ["<input type=hidden name=goto value=" ++ ?SECTION_GAME ++ ">"
         "<input type=hidden name=game value=" ++ integer_to_list(GameID) ++ ">"],
    HistBtn =
        fun(_, _, false) -> "";
           (Caption, LinkStep0, _Enabled) ->
                LinkStep =
                    if is_integer(LinkStep0) -> integer_to_list(LinkStep0);
                       true -> ""
                    end,
                tag(form, ["method=get", "action='/'"],
                    Hiddens ++
                        "<input type=hidden name=step value=" ++ LinkStep ++ ">"
                    "<input type=submit class=hb value='" ++ Caption ++ "'>")
        end,
    tag(
      table, ["cellpadding=0", "cellspacing=0", "width='100%'"],
      tr(
        [tag(td, ["class=hbc"],
             tag(form, ["method=get", "action='/'"],
                 Hiddens ++
                     "<input type=submit class=hb value='&#8635;'>")) |
         case {Step, IsLast} of
             {0, true} -> [];
             _ ->
                 [tag(td, ["class=hbc"], HistBtn("&lt;&lt;", 0, Step > 0)),
                  tag(td, ["class=hbc"], HistBtn("&lt;", Step - 1, Step > 0)),
                  tag(td, ["class=hbc"], HistBtn("&gt;", Step + 1, not IsLast)),
                  tag(td, ["class=hbc"], HistBtn("&gt;&gt;", last, not IsLast))]
         end])).

%% @doc
-spec cell(Board :: any(),
           C :: any(),
           R :: any()) ->
                  any().
cell(Board, C, R) ->
    element(C, element(8 - R + 1, Board)).

%% @doc
-spec game_history(Step :: non_neg_integer() | last,
                   GameID :: echessd_game:echessd_game_id(),
                   History :: any()) ->
                          HTML :: iolist().
game_history(last, GameID, History) ->
    game_history(length(History), GameID, History);
game_history(CurStep, GameID, History) ->
    tag(table, ["cellpadding=0", "cellspacing=0", "border=0"],
        game_history(CurStep, 1, integer_to_list(GameID), History)).
game_history(CurStep, N, GameID, [PlyW, PlyB | Tail]) ->
    ghc(
      N, game_history_itemlink(GameID, CurStep, (N - 1) * 2 + 1, PlyW),
      game_history_itemlink(GameID, CurStep, (N - 1) * 2 + 2, PlyB)) ++
        game_history(CurStep, N + 1, GameID, Tail);
game_history(CurStep, N, GameID, [PlyW]) ->
    ghc(N, game_history_itemlink(GameID, CurStep, (N - 1) * 2 + 1, PlyW), "");
game_history(_, _, _, _) -> "".

%% @doc
-spec ghc(N :: any(), StrW :: any(), StrB :: any()) -> HTML :: iolist().
ghc(N, StrW, StrB) ->
    tr(
      [tag(td, ["valign=bottom"], tt(integer_to_list(N) ++ ".&nbsp;")),
       tag(td, ["valign=bottom"], StrW), td("&nbsp;"),
       tag(td, ["valign=bottom"], StrB)]).

%% @doc
-spec game_history_itemlink(GameID :: echessd_game:echessd_game_id(),
                            CurStep :: non_neg_integer() | last,
                            Step :: non_neg_integer() | last,
                            Ply :: any()) ->
                                   HTML :: iolist().
game_history_itemlink(GameID, CurStep, Step, Ply) ->
    {Coords, Comment} =
        case Ply of
            {Coords0, Meta} ->
                {echessd_lib:escape_html_entities(
                   case proplists:get_value(notation, Meta) of
                       [_ | _] = Notation -> Notation;
                       _ -> Coords0
                   end),
                 echessd_lib:escape_html_entities(
                   proplists:get_value(comment, Meta, ""))};
            _ -> {Ply, ""}
        end,
    tag(
      a,
      ["title='" ++ Comment ++ "'",
       "href='/?goto=" ++ ?SECTION_GAME ++
           "&game=" ++ GameID ++
           "&step=" ++ integer_to_list(Step) ++ "'"],
      tt(if CurStep == Step -> b(Coords);
            true -> Coords
         end)) ++
        case Comment of
            [_ | _] -> tag(sup, "*");
            _ -> ""
        end.

%% @doc
-spec cseq(Direction :: 1 | -1) -> [1..8].
cseq(1) ->
    lists:seq(1, 8);
cseq(-1) ->
    lists:seq(8, 1, -1).

%% @doc
-spec inply(Ply :: nonempty_string(),
            Cell :: nonempty_string()) ->
                   boolean().
inply([A, B, _C, _D | _], [A, B]) ->
    true;
inply([_A, _B, C, D | _], [C, D]) ->
    true;
inply(_, _) ->
    false.

%% @doc
-spec captures(Captures :: list()) -> HTML :: iolist().
captures([_ | _] = Captures) ->
    tag("table", ["cellpadding=0", "cellspacing=0"],
        case [chessman_(F) || {?black, _} = F <- Captures] of
            [_ | _] = Black ->
                tr(tag(td, ["class=captures"],
                       ["&#" ++ integer_to_list(I) ++ ";" ||
                           I <- lists:sort(Black)]));
            _ -> ""
        end ++
        case [chessman_(F) || {?white, _} = F <- Captures] of
            [_ | _] = White ->
                tr(tag(td, ["class=captures"],
                       ["&#" ++ integer_to_list(I) ++ ";" ||
                           I <- lists:sort(White)]));
            _ -> ""
        end);
captures(_) -> "".

-type chessman() ::
        ?wpawn | ?wrook | ?wknight | ?wbishop | ?wqueen | ?wking |
        ?bpawn | ?brook | ?bknight | ?bbishop | ?bqueen | ?bking.

%% @doc
-spec chessman(Chessman :: chessman() | ?empty) -> HTML :: iolist().
chessman(?empty) ->
    "&nbsp;";
chessman(Chessman) ->
    "&#" ++ integer_to_list(chessman_(Chessman)) ++ ";".

%% @doc
-spec chessman_(Chessman :: chessman()) -> Codepoint :: pos_integer().
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

%% @doc
-spec navig_links(List :: list()) -> HTML :: iolist().
navig_links(List) ->
    navig_links(List, []).

%% @doc
-spec navig_links(List :: list(), Options :: list()) -> HTML :: iolist().
navig_links(List, Options) ->
    tag("div", ["class=navig"],
        proplists:get_value(prepend, Options, "") ++
            case List of
                [_ | _] ->
                    "[&nbsp;" ++
                        string:join(
                          lists:map(
                            fun({[_ | _] = URL, [_ | _] = Caption}) ->
                                    a(URL, Caption);
                               ({_, [_ | _] = Caption}) -> Caption
                            end, List), "&nbsp;|&nbsp;") ++
                        "&nbsp;]";
                _ -> ""
            end).

%% @doc
-spec navigation(Session :: #session{}) -> HTML :: iolist().
navigation(Session) when is_list(Session#session.username) ->
    navig_links(
      [{url([]), gettext(Session, txt_home, [])},
       {url([{?Q_GOTO, ?SECTION_USERS}]), gettext(Session, txt_users, [])},
       {url([{?Q_GOTO, ?SECTION_EXIT}]), gettext(Session, txt_logout, [])}]);
navigation(_Session) ->
    %% not logged in
    "".

%% @doc
-spec game_navigation(Session :: #session{},
                      GameID :: echessd_game:echessd_game_id(),
                      ShowEndGameLinks :: boolean()) ->
                             HTML :: iolist().
game_navigation(Session, GameID, ShowEndGameLinks) ->
    Links =
        [{url([{?Q_GOTO, ?SECTION_DRAW_CONFIRM},
               {?Q_GAME, GameID}]), gettext(Session, txt_req_draw, [])},
         {url([{?Q_GOTO, ?SECTION_GIVEUP_CONFIRM},
               {?Q_GAME, GameID}]), gettext(Session, txt_do_giveup, [])}],
    navig_links(
      case get(username) of
          [_ | _] ->
              [{"/", gettext(Session, txt_home, [])}] ++
                  if ShowEndGameLinks -> Links;
                     true -> ""
                  end ++
                  [{url([{?Q_GOTO, ?SECTION_EXIT}]),
                    gettext(Session, txt_logout, [])}];
          _ -> [{"/", gettext(Session, txt_authenticate, [])}]
      end,
      [{prepend, gettext(Session, txt_game, []) ++ " #" ++
            integer_to_list(GameID) ++ ": "}]).

%% @doc
-spec format_error(Session :: #session{},
                   Error :: {error, Reason :: any()} |
                            {wrong_move, Reason :: any()} |
                            any()) ->
                          PlainErrorMessage :: iolist().
format_error(Session, {error, Reason}) ->
    format_error(Session, Reason);
format_error(Session, {wrong_move, Reason}) ->
    gettext(Session, txt_badmove, []) ++
        case Reason of
            check ->
                gettext(Session, txt_king_in_check, []);
            badmove ->
                "";
            friendly_fire ->
                "";
            _ ->
                format_error(Session, Reason)
        end;
format_error(_Session, Term) ->
    io_lib:format("~120p", [Term]).

%% @doc
-spec warning(Session :: #session{}, TextID :: atom(), Args :: list()) ->
                     HTML :: iolist().
warning(Session, TextID, Args) ->
    tag("div", ["class=warning"], gettext(Session, TextID, Args)).

%% @doc
-spec gettext(Session :: #session{},
              FormatTextID :: atom(),
              Args :: list()) -> iolist().
gettext(Session, TextID, []) ->
    echessd_lang:gettext(TextID, Session#session.language);
gettext(Session, TextID, Args) ->
    io_lib:format(gettext(Session, TextID, []), Args).

%% @equiv echessd_query_parser:encode(Query)
%% @doc Generate Echessd URL.
-spec url(Query :: echessd_query_parser:http_query()) -> iolist().
url(URL) ->
    echessd_query_parser:encode(URL).
