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
    redirection/2
   ]).

-include("echessd.hrl").

-record(
   gameinfo,
   {id :: echessd_game:id(),
    board :: echessd_game:board(),
    captures :: [echessd_game:chessman()],
    status :: echessd_game:final_status(),
    full_history :: echessd_game:history(),
    full_history_len :: non_neg_integer(),
    history :: echessd_game:history(),
    history_len :: non_neg_integer(),
    step :: echessd_query_parser:step(),
    is_last_step :: boolean(),
    current_player_name :: nonempty_string(),
    current_color :: echessd_game:color(),
    idle_player_name :: nonempty_string(),
    idle_color :: echessd_game:color(),
    is_my_game :: boolean(),
    is_my_turn :: boolean(),
    info :: echessd_game:info()
   }).

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
           echessd_user:get_value(?ui_fullname, UserInfo)), "<br>",
     select(Session, ?Q_EDIT_TIMEZONE, txt_timezone,
            echessd_user:get_value(?ui_timezone, UserInfo),
            echessd_lib:administrative_offsets()), "<br>",
     select(Session, ?Q_EDIT_LANGUAGE, txt_predit_lang,
            Session#session.language, echessd_lang:list()), "<br>",
     select(Session, ?Q_EDIT_STYLE, txt_style,
            Session#session.style,
            [{StyleID, gettext(Session, StyleCaptionTextID, [])} ||
                {StyleID, StyleCaptionTextID} <- echessd_styles:list()]),
     "<br>",
     input(Session, ?Q_EDIT_JID, txt_jid,
           echessd_user:get_value(?ui_jid, UserInfo)) ++ " (" ++
         gettext(Session, txt_rnu_optional, []) ++ ")<br>",
     checkbox(Session, "enot", ?Q_EDIT_NOTIFY, txt_notify,
              echessd_user:get_value(?ui_notify, UserInfo)), "<br>",
     checkbox(Session, "sil", ?Q_EDIT_SHOW_IN_LIST, txt_rnu_show_in_list,
              echessd_user:get_value(?ui_show_in_list, UserInfo)), "<br>",
     checkbox(Session, "sh", ?Q_EDIT_SHOW_HISTORY, txt_rnu_show_history,
              echessd_user:get_value(?ui_show_history, UserInfo)), "<br>",
     checkbox(Session, "sc", ?Q_EDIT_SHOW_COMMENT, txt_rnu_show_comment,
              echessd_user:get_value(?ui_show_comment, UserInfo)), "<br>",
     checkbox(Session, "autoref", ?Q_EDIT_AUTO_REFRESH, txt_auto_refresh,
              echessd_user:get_value(?ui_auto_refresh, UserInfo)), "<br>",
     input(Session, ?Q_EDIT_AUTO_PERIOD, txt_auto_refresh_period,
           echessd_user:get_value(?ui_auto_refresh_period, UserInfo)), "<br>",
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
     password(Session, ?Q_EDIT_PASSWORD0, txt_passwd_passw), "<br>",
     password(Session, ?Q_EDIT_PASSWORD1, txt_passwd_passw_new), "<br>",
     password(Session, ?Q_EDIT_PASSWORD2, txt_passwd_passw_new_confirm),
     "<br>",
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
     case echessd_user:get_value(?ui_auto_refresh, UserInfo) of
         true ->
             tag(
               script, [],
               ["setTimeout(\"document.location.href='/'\",",
                integer_to_list(
                  echessd_user:get_value(
                    ?ui_auto_refresh_period, UserInfo) * 1000), ")"]);
         _ -> ""
     end,
     html_page_footer()].

%% @doc Make 'registered users list' page.
-spec users(Session :: #session{}) -> HTML :: iolist().
users(Session) ->
    Users = lists:usort(echessd_user:list()) -- [Session#session.username],
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
        Username ->
            {ok, UserInfo} = echessd_user:getprops(Username),
            Title = gettext(Session, txt_user, []) ++ " '" ++ Username ++ "'",
            [html_page_header(Session, "echessd - " ++ Title, [{h1, Title}]),
             navigation(Session), "<br>",
             user_info(Session, Username, UserInfo), "<br>",
             user_games(Session, Username, UserInfo, false), "<br>",
             newgame_link(Session, Username),
             html_page_footer()]
    end.

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
     "<br>",
     checkbox(Session, "prv", ?Q_PRIVATE, txt_ng_private, false), "<br>",
     submit(Session, txt_ng_ok_button),
     "</form>",
     html_page_footer()].

%% @doc Make 'game' page.
-spec game(Session :: #session{},
           Query :: echessd_query_parser:http_query()) ->
                  HTML :: iolist().
game(Session, Query) ->
    GameID = proplists:get_value(?Q_GAME, Query),
    Step = proplists:get_value(?Q_STEP, Query, last),
    case get_extended_game_info(Session, GameID, Step) of
        {ok, GameInfo} ->
            [html_page_header(
               Session,
               if GameInfo#gameinfo.is_my_turn ->
                       ["echessd: ", gettext(Session, txt_your_move, [])];
                  true ->
                       ["echessd: ", gettext(Session, txt_game, []), " #",
                        integer_to_list(GameInfo#gameinfo.id)]
               end, []),
             game_navigation(Session, GameInfo),
             game_title_message(Session, GameInfo),
             case echessd_user:get_value(
                    ?ui_show_history, Session#session.userinfo) of
                 true ->
                     table(
                       ["cellpadding=0", "cellspacing=0", "border=0"],
                       [[td(["valign=top"], chess_table(Session, GameInfo)),
                         td(["valign=top"], game_history(GameInfo))]]);
                 false ->
                     chess_table(Session, GameInfo)
             end,
             if GameInfo#gameinfo.is_my_turn ->
                     "";
                true ->
                     autorefresh_hook(Session, GameInfo)
             end,
             html_page_footer()];
        ErrorContent ->
            ErrorContent
    end.

%% @doc Return 'true' if there is a need to rotate the chess board.
-spec need_to_rotate(Session :: #session{}, GameInfo :: #gameinfo{}) ->
                            boolean().
need_to_rotate(Session, GameInfo) when GameInfo#gameinfo.is_my_game ->
    %% its my game
    if GameInfo#gameinfo.idle_player_name ==
       GameInfo#gameinfo.current_player_name ->
            %% test game
            GameInfo#gameinfo.current_color == ?black;
       Session#session.username == GameInfo#gameinfo.current_player_name ->
            %% my turn
            GameInfo#gameinfo.current_color == ?black;
       Session#session.username == GameInfo#gameinfo.idle_player_name ->
            %% not my turn
            GameInfo#gameinfo.idle_color == ?black
    end;
need_to_rotate(_Session, _GameInfo) ->
    false.

%% @doc
-spec is_my_game(Session :: #session{},
                 GameInfo :: echessd_game:info()) -> boolean().
is_my_game(Session, GameInfo)
  when Session#session.username /= undefined ->
    length(
      [z || {?gi_users, [_ | _] = Watchers} <- GameInfo,
            {Name, Color} <- Watchers,
            Name == Session#session.username,
            lists:member(Color, [?white, ?black])]) > 0;
is_my_game(_Session, _GameInfo) ->
    false.

%% @doc
-spec game_title_message(Session :: #session{}, GameInfo :: #gameinfo{}) ->
                                HTML :: iolist().
game_title_message(Session, GameInfo) ->
    case GameInfo#gameinfo.status of
        ?gs_checkmate ->
            h2(gettext(Session, txt_gt_over_checkmate, [winnerlink(GameInfo)]));
        ?gs_give_up ->
            h2(gettext(Session, txt_gt_over_giveup, [winnerlink(GameInfo)]));
        ?gs_draw_stalemate ->
            h2(gettext(Session, txt_gt_over_stalemate, []));
        ?gs_draw_agreement ->
            h2(gettext(Session, txt_gt_over_agreement, []));
        ?gs_alive when GameInfo#gameinfo.is_my_game ->
            case proplists:get_value(
                   ?gi_draw_request_from, GameInfo#gameinfo.info) of
                undefined ->
                    "";
                Iam when Iam == Session#session.username ->
                    warning(Session, txt_gt_youre_drawing, []);
                OpponentName ->
                    warning(Session, txt_gt_opponent_drawing,
                            [userlink(OpponentName)])
            end;
        ?gs_alive ->
            ""
    end.

%% @doc
-spec chess_table(Session :: #session{}, GameInfo :: #gameinfo{}) ->
                         HTML :: iolist().
chess_table(Session, GameInfo) ->
    {LastPlyCoords, Comment} =
        case lists:reverse(GameInfo#gameinfo.history) of
            [{LastPlyCoords0, PlyInfo} | _] ->
                {LastPlyCoords0,
                 proplists:get_value(?pi_comment, PlyInfo, "")};
            [] ->
                {undefined, ""}
        end,
    Hints =
        if GameInfo#gameinfo.is_my_turn andalso
           GameInfo#gameinfo.is_last_step ->
                echessd_game:hint(
                  proplists:get_value(?gi_type, GameInfo#gameinfo.info),
                  GameInfo#gameinfo.history);
           true -> []
        end,
    ActiveCells =
        lists:usort(
          lists:append([[[A, B], [C, D]] || [A, B, C, D] <- Hints])),
    IsShowComment =
        echessd_user:get_value(?ui_show_comment, Session#session.userinfo),
    [chess_board(Session, GameInfo, ActiveCells, LastPlyCoords),
     case {IsShowComment, Comment} of
         {true, [_ | _]} ->
             tag(p, gettext(Session, txt_comment, []) ++ ": " ++ Comment);
         _ -> ""
     end,
     if GameInfo#gameinfo.is_my_turn andalso
        GameInfo#gameinfo.is_last_step ->
             move_form(
               Session, GameInfo#gameinfo.id,
               Hints, ActiveCells, LastPlyCoords);
        true -> ""
     end,
     captures(GameInfo#gameinfo.captures)].

%% @doc Make move form.
-spec move_form(Session :: #session{}, GameID :: echessd_game:id(),
                Hints :: [echessd_game:ply_coords()],
                ActiveCells :: [nonempty_string()],
                LastPly :: echessd_game:ply_coords()) ->
                       HTML :: iolist().
move_form(Session, GameID, Hints, ActiveCells, LastPly) ->
    [tag(script, ["src='/res/echessd.js'"], ""),
     tag(script, js_init(Hints, ActiveCells, LastPly)),
     "<form method=post>",
     hidden(?Q_GOTO, ?SECTION_MOVE),
     hidden(?Q_GAME, GameID),
     gettext(Session, txt_move_caption, []) ++ ":&nbsp;" ++
         "<input name=move type=text size=5 id=edmv>",
     submit(Session, txt_move_ok_button),
     "<input type=reset class=btn value='" ++
         gettext(Session, txt_move_reset_button, []) ++
         "' onclick='clr();'><br>",
     input(Session, ?Q_COMMENT, txt_move_comment_caption, ""),
     "</form><br>"].

%% @doc
-spec autorefresh_hook(Session :: #session{}, GameInfo :: #gameinfo{}) ->
                              HTML :: iolist().
autorefresh_hook(Session, GameInfo) ->
    UserInfo = Session#session.userinfo,
    case echessd_user:get_value(?ui_auto_refresh, UserInfo) of
        true ->
            AutoRefreshPeriod =
                echessd_user:get_value(?ui_auto_refresh_period, UserInfo),
            tag(
              script,
              ["setTimeout(\"document.location.href='",
               url([{?Q_GOTO, ?SECTION_GAME},
                    {?Q_GAME, GameInfo#gameinfo.id},
                    {?Q_STEP, GameInfo#gameinfo.step}]),
               "'\",", integer_to_list(AutoRefreshPeriod * 1000), ")"]);
        false ->
            ""
    end.

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
     hidden(?Q_GOTO, ?SECTION_DRAW),
     hidden(?Q_GAME, GameID),
     submit(Session, txt_draw_button),
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
    {ok, GameInfo} = echessd_game:getprops(GameID),
    {_CurrentPlayerName, _CurrentColor, IdlePlayerName, _IdleColor} =
        echessd_game:get_players(GameInfo),
    Title = gettext(Session, txt_giveup_confirm_title, []),
    [html_page_header(Session, "echessd - " ++ Title, [{h1, Title}]),
     warning(Session, txt_giveup_confirm_text, [userlink(IdlePlayerName)]),
     "<form method=post>",
     hidden(?Q_GOTO, ?SECTION_GIVEUP),
     hidden(?Q_GAME, GameID),
     submit(Session, txt_giveup_button),
     "</form>",
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
    ?MODULE:error(Session, gettext(Session, txt_access_denied, [])).

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
-spec get_extended_game_info(Session :: #session{},
                             GameID :: echessd_game:id(),
                             Step :: echessd_query_parser:step()) ->
                                    {ok, ExtendedGameInfo :: #gameinfo{}} |
                                    (ErrorPage :: iolist()).
get_extended_game_info(Session, GameID, Step) ->
    case fetch_game(Session, GameID) of
        {ok, GameInfo} ->
            {CurrentPlayerName, CurrentColor, IdlePlayerName, IdleColor} =
                echessd_game:get_players(GameInfo),
            FullHistory = proplists:get_value(?gi_history, GameInfo, []),
            FullHistoryLen = length(FullHistory),
            Status = proplists:get_value(?gi_status, GameInfo, ?gs_alive),
            {History, IntStep, IsLastStep} =
                if Step == last orelse Step >= FullHistoryLen ->
                        {FullHistory, FullHistoryLen, true};
                   true ->
                        {lists:sublist(FullHistory, Step), Step, false}
                end,
            {Board, Captures, _TurnColor} =
                echessd_game:from_scratch(
                  proplists:get_value(?gi_type, GameInfo), History),
            IsMyTurn = Status == ?gs_alive andalso
                CurrentPlayerName == Session#session.username,
            {ok,
             #gameinfo{id = GameID,
                       board = Board,
                       captures = Captures,
                       status = Status,
                       full_history = FullHistory,
                       full_history_len = FullHistoryLen,
                       history = History,
                       history_len = length(History),
                       step = IntStep,
                       is_last_step = IsLastStep,
                       current_player_name = CurrentPlayerName,
                       current_color = CurrentColor,
                       idle_player_name = IdlePlayerName,
                       idle_color = IdleColor,
                       is_my_game = is_my_game(Session, GameInfo),
                       is_my_turn = IsMyTurn,
                       info = GameInfo
                      }};
        ErrorPage ->
            ErrorPage
    end.

%% @doc
-spec fetch_game(Session :: #session{},
                 GameID :: echessd_game:id()) ->
                        {ok, GameInfo :: echessd_game:info()} |
                        (ErrorPagePayload :: iolist()).
fetch_game(Session, GameID) ->
    case echessd_game:getprops(GameID) of
        {ok, GameInfo} ->
            case proplists:is_defined(?gi_private, GameInfo) of
                true ->
                    case is_my_game(Session, GameInfo) of
                        true ->
                            {ok, GameInfo};
                        _ ->
                            ?MODULE:error(
                               Session,
                               gettext(Session, txt_game_fetch_error, []) ++
                                   ":~n~p", [GameID, {no_such_game, GameID}])
                    end;
                false ->
                    case proplists:is_defined(?gi_acknowledged, GameInfo) of
                        true ->
                            {ok, GameInfo};
                        false ->
                            ?MODULE:error(
                               Session,
                               gettext(
                                 Session, txt_game_not_confirmed_error,
                                 [GameID]))
                    end
            end;
        {error, Reason} ->
            ?MODULE:error(
               Session,
               gettext(Session, txt_game_fetch_error, []) ++ ":~n~p",
               [GameID, Reason])
    end.

%% @doc Initialisation JavaScript code for hinting.
-spec js_init(Hints :: [echessd_game:ply_coords()],
              ActiveCells :: [CellCoord :: nonempty_string()],
              LastPlyCoords :: echessd_game:ply_coords() | undefined) ->
                     HTML :: iolist().
js_init(Hints, ActiveCells, LastPlyCoords) ->
    GroupedHints = group_hints(Hints),
    ["var ACs = [",
     string:join(["'" ++ C ++ "'" || C <- ActiveCells], ","),
     "];\nvar I1s = [",
     string:join(["'" ++ I1 ++ "'" || {I1, _} <- GroupedHints], ","),
     "];\nvar I2s = [",
     string:join(
       [[$[, string:join([[$', I2, $'] || I2 <- L], ","), $]] ||
           {_, L} <- GroupedHints], ","),
     "];\nvar LPs = [",
     case LastPlyCoords of
         [A, B, C, D] ->
             [$', [A, B], "','", [C, D], $'];
         _ -> ""
     end, "];\n"].

%% @doc
-spec group_hints(PlyCoords :: [echessd_game:ply_coords()]) ->
                         [{StartCellCoord :: nonempty_string(),
                           [DestinationCellCoord :: nonempty_string()]}].
group_hints(PlyCoords) ->
    StartCells = lists:usort([[A, B] || [A, B | _] <- PlyCoords]),
    [{StartCell,
      [[C, D] || [A, B, C, D] <- PlyCoords, [A, B] == StartCell]}
     || StartCell <- StartCells].

%% @doc
-spec log_reg_page(Session :: #session{},
                   Section :: echessd_query_parser:section(),
                   Title :: string(),
                   HtmlPagePayload :: iolist()) -> HtmlPage :: iolist().
log_reg_page(Session, Section, Title, Content) ->
    [html_page_header(Session, Title, []),
     navig_links(
       lists:map(
         fun(LangID) ->
                 {echessd_query_parser:encode(
                    [{?Q_GOTO, Section}, {?Q_LANG, LangID}]),
                  echessd_query_parser:encode(?Q_LANG, LangID)}
         end, [LangID || {LangID, _} <- echessd_lang:list()])),
     h1(Title), Content, html_page_footer()].

%% @doc Make 'user info' table.
-spec user_info(Session :: #session{},
                Username :: echessd_user:name(),
                UserInfo :: echessd_user:info()) ->
                       HTML :: iolist().
user_info(Session, Username, UserInfo) ->
    table(
      ["cellpadding=0", "cellspacing=0"],
      [[td(b(K ++ ":&nbsp;")), td(V)] ||
          {K, V} <- user_info_cells(Session, Username, UserInfo)]).

%% @doc Return the cells for the 'user info' table.
-spec user_info_cells(Session :: #session{},
                      Username :: echessd_user:name(),
                      UserInfo :: echessd_user:info()) ->
                             [{CellCaption :: iolist(),
                               CellValue :: iolist()}].
user_info_cells(Session, Username, UserInfo) ->
    lists:flatmap(
      fun(?ui_login) ->
              [{gettext(Session, txt_login, []), Username}];
         (?ui_fullname = Key) ->
              [{gettext(Session, txt_fullname, []),
                case echessd_user:get_value(Key, UserInfo) of
                    [_ | _] = Value ->
                        echessd_lib:escape_html_entities(Value);
                    _ -> gettext(Session, txt_not_sure, [])
                end}];
         (?ui_created = Key) ->
              [{gettext(Session, txt_registered, []),
                case echessd_user:get_value(Key, UserInfo) of
                    Value when ?is_now(Value) ->
                        echessd_lib:timestamp(
                          Value, Session#session.timezone);
                    _ -> gettext(Session, txt_unknown, [])
                end}];
         (?ui_timezone = Key) ->
              [{gettext(Session, txt_timezone, []),
                echessd_lib:time_offset_to_list(
                  echessd_user:get_value(Key, UserInfo))}];
         (?ui_language = Key) ->
              [{gettext(Session, txt_language, []),
                proplists:get_value(
                  echessd_user:get_value(Key, UserInfo),
                  echessd_lang:list())}];
         (_) -> []
      end, [?ui_login, ?ui_fullname, ?ui_created, ?ui_timezone, ?ui_language]).

%% @doc
-spec user_games(Session :: #session{},
                 Username :: echessd_user:name(),
                 UserInfo :: echessd_user:info(),
                 ShowNotAcknowledged :: boolean()) ->
                        HTML :: iolist().
user_games(Session, Username, UserInfo, ShowNotAcknowledged) ->
    %% fetch all user games info
    UserGames =
        lists:flatmap(
          fun(GameID) ->
                  {ok, GameInfo} = echessd_game:getprops(GameID),
                  case not proplists:is_defined(?gi_private, GameInfo)
                      orelse is_my_game(Session, GameInfo) of
                      true ->
                          [{GameID, GameInfo}];
                      false ->
                          []
                  end
          end, echessd_user:get_value(?ui_games, UserInfo)),
    %% split not acknowledged
    {Acknowledged, NotAcknowledged} =
        lists:partition(
          fun({_GameID, GameInfo}) ->
                  proplists:is_defined(?gi_acknowledged, GameInfo)
          end, UserGames),
    %% split finished
    {Active, Finished} =
        lists:partition(
          fun({_GameID, GameInfo}) ->
                  proplists:get_value(?gi_status, GameInfo) == ?gs_alive
          end, Acknowledged),
    [user_games_active(Session, Username, Active),
     if ShowNotAcknowledged ->
             user_games_not_acknowledged(Session, Username, NotAcknowledged);
        true ->
             []
     end,
     user_games_finished(Session, Username, Finished)].

%% @doc Generate a list of active games for the user.
-spec user_games_active(Session :: #session{},
                        Username :: echessd_user:name(),
                        [{GameID :: echessd_game:id(),
                          GameInfo :: echessd_game:info()}]) ->
                               HTML :: iolist().
user_games_active(_Session, _Username, []) ->
    [];
user_games_active(Session, Username, List) ->
    [h2(gettext(Session, txt_user_games, []) ++ ":") |
     [[user_games_line(Session, Username, GameID, GameInfo), "<br>"] ||
         {GameID, GameInfo} <- List]].

%% @doc Generate a list of finished games for the user.
-spec user_games_finished(Session :: #session{},
                          Username :: echessd_user:name(),
                          [{GameID :: echessd_game:id(),
                            GameInfo :: echessd_game:info()}]) ->
                                 HTML :: iolist().
user_games_finished(_Session, _Username, []) ->
    [];
user_games_finished(Session, Username, List) ->
    [h2(gettext(Session, txt_user_ended_games, []) ++ ":") |
     [[user_games_line(Session, Username, GameID, GameInfo), "<br>"] ||
        {GameID, GameInfo} <- List]].

%% @doc Generate a line of game list for the user.
-spec user_games_line(Session :: #session{},
                      Owner :: echessd_user:name(),
                      GameID :: echessd_game:id(),
                      GameInfo :: echessd_game:info()) ->
                             HTML :: iolist().
user_games_line(Session, Owner, GameID, GameInfo) ->
    Watchers = proplists:get_value(?gi_users, GameInfo, []),
    OwnerColor =
        hd([Color || {Name, Color} <- Watchers,
                     lists:member(Color, [?white, ?black]),
                     Name == Owner]),
    {OpponentName, OpponentColor} =
        hd([Player || {_Name, Color} = Player <- Watchers,
                      lists:member(Color, [?white, ?black]),
                      Color /= OwnerColor]),
    {CurrentPlayerName, _CurrentColor} =
        echessd_game:current_player(GameInfo),
    IsTest = Owner == OpponentName,
    ["* ", gamelink(GameID),
     if IsTest ->
             [$\s, gettext(Session, txt_test_game, [])];
        true ->
             [$\s, chessman({OwnerColor, ?king}), $\s,
              gettext(Session, txt_vs, []),
              $\s, userlink(OpponentName), $\s,
              chessman({OpponentColor, ?king})]
     end,
     case proplists:get_value(?gi_status, GameInfo) of
         ?gs_alive when Session#session.username == CurrentPlayerName ->
             " !!!";
         ?gs_alive ->
             "";
         ?gs_checkmate when IsTest ->
             [" - ", gettext(Session, txt_checkmate, [])];
         ?gs_checkmate ->
             case proplists:get_value(?gi_winner, GameInfo) of
                 Owner ->
                     [" - ", gettext(Session, txt_win, [])];
                 _ ->
                     [" - ", gettext(Session, txt_loose, [])]
             end;
         ?gs_give_up when IsTest ->
             [" - ", gettext(Session, txt_gived_up, [])];
         ?gs_give_up ->
             case proplists:get_value(?gi_winner, GameInfo) of
                 Owner ->
                     [" - ", gettext(Session, txt_win_giveup, [])];
                 _ ->
                     [" - ", gettext(Session, txt_loose_giveup, [])]
             end;
         Draw when Draw == ?gs_draw_stalemate; Draw == ?gs_draw_agreement ->
             [" - ", gettext(Session, txt_draw, [])]
     end].

%% @doc Generate a list of not acknowledged games for the user.
-spec user_games_not_acknowledged(Session :: #session{},
                                  Username :: echessd_user:name(),
                                  [{GameID :: echessd_game:id(),
                                    GameInfo :: echessd_game:info()}]) ->
                                         HTML :: iolist().
user_games_not_acknowledged(_Session, _Username, []) ->
    [];
user_games_not_acknowledged(Session, Username, List) ->
    [h2(gettext(Session, txt_unconf_games, []) ++ ":") |
     [[user_games_not_acknowledged_line(
         Session, Username, GameID, GameInfo), "<br>"] ||
         {GameID, GameInfo} <- List]].

%% @doc Generate a line of not acknowledged game list for the user.
-spec user_games_not_acknowledged_line(Session :: #session{},
                                       Owner :: echessd_user:name(),
                                       GameID :: echessd_game:id(),
                                       GameInfo :: echessd_game:info()) ->
                                              HTML :: iolist().
user_games_not_acknowledged_line(Session, Owner, GameID, GameInfo) ->
    GamePlayers =
        [{N, C} || {N, C} <- proplists:get_value(?gi_users, GameInfo, []),
                   lists:member(C, [?white, ?black])],
    UniquePlayerNames = lists:usort([N || {N, _} <- GamePlayers]),
    ["* #", integer_to_list(GameID), $\s,
     case proplists:get_value(?gi_creator, GameInfo) of
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
     end, $\s,
     tag(a, ["href='" ++
                 url([{?Q_GOTO, ?SECTION_DENYGAME}, {?Q_GAME, GameID}]) ++
                 "'"],
         gettext(Session, txt_game_deny, []))].

%% @doc
-spec html_page_header(Session :: #session{},
                       Title :: iolist(), Options :: list()) ->
                              HtmlPageHeader :: iolist().
html_page_header(Session, Title, Options) ->
    {_, StyleFilename} = echessd_styles:get(Session#session.style),
    ["<html>\n\n"
     "<head>\n"
     "<meta http-equiv='Content-Type' content='text/html; charset=utf-8'>\n"
     "<meta http-equiv='Content-Style-Type' content='text/css'>\n"
     "<meta http-equiv='Content-Script-Type' content='text/javascript'>\n"
     "<title>", Title, "</title>\n"
     "<link rel='stylesheet' href='/res/", StyleFilename, "'>\n"
     "</head>\n\n"
     "<body>\n\n",
     case [S || {h1, S} <- Options] of
         [H1 | _] -> h1(H1);
         _ -> ""
     end,
     case get(error) of
         undefined -> "";
         Error ->
             tag("div", ["class=error"],
                 pre(gettext(Session, txt_error, []) ++ ": " ++
                         format_error(Session, Error)))
     end].

%% @doc
-spec html_page_footer() -> HtmlPageFooter :: iolist().
html_page_footer() ->
    "\n\n</body>\n</html>\n".

%% @doc
-spec h1(String :: iolist()) -> HTML :: iolist().
h1(String) ->
    tag(h1, String).

%% @doc
-spec h2(String :: iolist()) -> HTML :: iolist().
h2(String) ->
    tag(h2, String).

%% @doc
-spec b(String :: iolist()) -> HTML :: iolist().
b(String) ->
    tag(b, String).

%% @doc
-spec tt(String :: iolist()) -> HTML :: iolist().
tt(String) ->
    tag(tt, String).

%% @doc
-spec table(Attrs :: [string()], Rows :: [Row :: iolist()]) ->
                   HTML :: iolist().
table(Attrs, Rows) ->
    tag(table, Attrs, [tr(Row) || Row <- Rows]).

%% @doc
-spec tr(String :: iolist()) -> HTML :: iolist().
tr(String) ->
    tag(tr, String).

%% @equiv td([], String)
%% @doc
-spec td(String :: iolist()) -> HTML :: iolist().
td(String) ->
    td([], String).

%% @doc
-spec td(Attrs :: [string()], String :: iolist()) -> HTML :: iolist().
td(Attrs, String) ->
    tag(td, Attrs, String).

%% @doc
-spec pre(String :: iolist()) -> HTML :: iolist().
pre(String) ->
    tag(pre, String).

%% @doc
-spec a(URL :: string(), Caption :: iolist()) -> HTML :: iolist().
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
-spec select_option(Value :: iolist(), Caption :: iolist(),
                    IsSelected :: boolean()) -> HTML :: iolist().
select_option(Value, Caption, IsSelected) ->
    ["<option value='", Value, "'",
     proplists:get_value(IsSelected, [{true, " selected"}], ""),
     ">", Caption, "</option>"].

%% @doc
-spec userlink(Username :: echessd_user:name()) -> HTML :: iolist().
userlink(Username) ->
    a(url([{?Q_GOTO, ?SECTION_USER}, {?Q_NAME, Username}]), Username).

%% @doc
-spec winnerlink(GameInfo :: #gameinfo{}) -> HTML :: iolist().
winnerlink(GameInfo) ->
    Winner = proplists:get_value(?gi_winner, GameInfo#gameinfo.info),
    case proplists:get_value(?gi_winner_color, GameInfo#gameinfo.info) of
        ?white ->
            [chessman(?wking), "&nbsp;", userlink(Winner)];
        ?black ->
            [chessman(?bking), "&nbsp;", userlink(Winner)]
    end.

%% @doc
-spec gamelink(GameID :: echessd_game:id()) -> HTML :: iolist().
gamelink(GameID) ->
    a(url([{?Q_GOTO, ?SECTION_GAME}, {?Q_GAME, GameID}]),
      ["#", echessd_query_parser:encode(?Q_GAME, GameID)]).

%% @doc
-spec newgame_link(Session :: #session{},
                   WithUsername :: echessd_user:name()) ->
                          HTML :: iolist().
newgame_link(Session, WithUsername) ->
    navig_links(
      [{url([{?Q_GOTO, ?SECTION_NEWGAME}, {?Q_USER, WithUsername}]),
        gettext(Session, txt_new_game_link, [])}]).

%% @doc
-spec chess_board(Session :: #session{}, GameInfo :: #gameinfo{},
                  ActiveCells :: [nonempty_string()],
                  LastPly :: echessd_game:ply_coords()) ->
                         HTML :: iolist().
chess_board(Session, GameInfo, ActiveCells, LastPly) ->
    {ColStep, RowStep} =
        case need_to_rotate(Session, GameInfo) of
            true -> {-1, 1};
            false -> {1, -1}
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
    table(
      ["cellpadding=0", "cellspacing=0"],
      [[td(""),
        [td(["class=crd_t"], tt([$a + C - 1])) || C <- cseq(ColStep)],
        td("")] |
       lists:map(
         fun(R) ->
                 [td(["class=crd_l"], tt([$1 + R - 1])),
                  lists:map(
                    fun(C) ->
                            Crd = [$a + C - 1, $1 + R - 1],
                            IsActive = lists:member(Crd, ActiveCells),
                            CellClass =
                                proplists:get_value(
                                  inply(LastPly, Crd),
                                  [{true, "lastply"}], "cell"),
                            td(["class=" ++ Color(C, R)] ++
                                   ExtraAction(Crd, IsActive),
                               tag("div",
                                   ["class=" ++ CellClass] ++
                                       if IsActive -> ["id=" ++ Crd];
                                          true -> []
                                       end,
                                   chessman(
                                     cell(GameInfo#gameinfo.board, C, R))))
                    end, cseq(ColStep)),
                  td(["class=crd_r"], tt([$1 + R - 1]))]
         end, cseq(RowStep))] ++
          [[td(""),
            [td(["class=crd_b"], tt([$a + C - 1])) || C <- cseq(ColStep)],
            td("")],
           [td(""),
            td(["colspan=8"], history_buttons(GameInfo)),
            td("")]]).

%% @doc
-spec history_buttons(GameInfo :: #gameinfo{}) -> HTML :: iolist().
history_buttons(GameInfo) ->
    HistBtn =
        fun(_, _, false) ->
                td(["class=hbc"], "");
           (Caption, LinkStep, _Enabled) ->
                td(["class=hbc"],
                   tag(form, ["method=get"],
                       [hidden(?Q_GOTO, ?SECTION_GAME),
                        hidden(?Q_GAME, GameInfo#gameinfo.id),
                        hidden(?Q_STEP, LinkStep),
                        "<input type=submit class=hb value='",
                        Caption, "'>"]))
        end,
    Step = GameInfo#gameinfo.step,
    IsLastStep = GameInfo#gameinfo.is_last_step,
    table(
      ["cellpadding=0", "cellspacing=0", "width='100%'"],
      [[td(["class=hbc"],
           tag(form, ["method=get"],
               [hidden(?Q_GOTO, ?SECTION_GAME),
                hidden(?Q_GAME, GameInfo#gameinfo.id),
                "<input type=submit class=hb value='&#8635;'>"])) |
        case {Step, IsLastStep} of
            {0, true} -> [];
            _ ->
                [HistBtn("&lt;&lt;", 0, Step > 0),
                 HistBtn("&lt;", Step - 1, Step > 0),
                 HistBtn("&gt;", Step + 1, not IsLastStep),
                 HistBtn("&gt;&gt;", last, not IsLastStep)]
        end]]).

%% @doc
-spec cell(Board :: echessd_game:board(),
           Column :: 1..8, Row :: 1..8) ->
                  Entry :: echessd_game:entry().
cell(Board, C, R) ->
    element(C, element(8 - R + 1, Board)).

%% @doc Generate game history table.
-spec game_history(GameInfo :: #gameinfo{}) -> HTML :: iolist().
game_history(GameInfo) when GameInfo#gameinfo.step == last ->
    game_history(GameInfo#gameinfo{step = GameInfo#gameinfo.history_len});
game_history(GameInfo) ->
    table(
      ["cellpadding=0", "cellspacing=0", "border=0"],
      game_history(
        GameInfo#gameinfo.step, _No = 1,
        GameInfo#gameinfo.id, GameInfo#gameinfo.full_history)).

%% @doc Helper for the game_history/1 fun.
-spec game_history(CurStep :: non_neg_integer(),
                   No :: pos_integer(), GameID :: echessd_game:id(),
                   History :: echessd_game:history()) ->
                          HtmlTableRows :: [Row :: iolist()].
game_history(CurStep, No, GameID, [PlyWhite, PlyBlack | HistoryTail]) ->
    [game_history_row(
       No,
       game_history_itemlink(GameID, CurStep, (No - 1) * 2 + 1, PlyWhite),
       game_history_itemlink(GameID, CurStep, (No - 1) * 2 + 2, PlyBlack)) |
     game_history(CurStep, No + 1, GameID, HistoryTail)];
game_history(CurStep, No, GameID, [PlyWhite]) ->
    [game_history_row(
       No, game_history_itemlink(GameID, CurStep, (No - 1) * 2 + 1, PlyWhite),
       "")];
game_history(_CurStep, _No, _GameID, []) ->
    [].

%% @doc Make a row for the game history table.
-spec game_history_row(No :: pos_integer(), WhitePlyLink :: iolist(),
                       BlackPlyLink :: iolist()) -> HTML :: iolist().
game_history_row(No, WhitePlyLink, BlackPlyLink) ->
    [td(["valign=bottom"], tt([integer_to_list(No), ".&nbsp;"])),
     td(["valign=bottom"], WhitePlyLink),
     td("&nbsp;"),
     td(["valign=bottom"], BlackPlyLink)].

%% @doc
-spec game_history_itemlink(GameID :: echessd_game:id(),
                            CurStep :: echessd_query_parser:step(),
                            Step :: echessd_query_parser:step(),
                            Ply :: echessd_game:ply()) ->
                                   HTML :: iolist().
game_history_itemlink(GameID, CurStep, Step, {_PlyCoords, PlyInfo}) ->
    Caption =
        echessd_lib:escape_html_entities(
          proplists:get_value(?pi_notation, PlyInfo)),
    Comment =
        echessd_lib:escape_html_entities(
          proplists:get_value(?pi_comment, PlyInfo, "")),
    tag(
      a,
      ["title='" ++ Comment ++ "'",
       "href='" ++
           url([{?Q_GOTO, ?SECTION_GAME},
                {?Q_GAME, GameID}, {?Q_STEP, Step}]) ++ "'"],
      [tt(if CurStep == Step ->
                  b(Caption);
             true ->
                  Caption
          end),
       case Comment of
           [_ | _] ->
               tag(sup, "*");
           _ -> ""
       end]).

%% @doc
-spec cseq(Direction :: 1 | -1) -> [1..8].
cseq(1) ->
    lists:seq(1, 8);
cseq(-1) ->
    lists:seq(8, 1, -1).

%% @doc
-spec inply(Ply :: echessd_game:ply_coords(),
            Cell :: nonempty_string()) -> boolean().
inply([A, B, _C, _D | _], [A, B]) ->
    true;
inply([_A, _B, C, D | _], [C, D]) ->
    true;
inply(_, _) ->
    false.

%% @doc
-spec captures(Captures :: [echessd_game:chessman()]) -> HTML :: iolist().
captures(Captures) ->
    table(
      ["cellpadding=0", "cellspacing=0"],
      [td(["class=captures"],
          lists:sort([chessman(F) || {?black, _} = F <- Captures])),
       td(["class=captures"],
          lists:sort([chessman(F) || {?white, _} = F <- Captures]))]).

%% @doc
-spec chessman(Chessman :: echessd_game:chessman() | ?empty) ->
                      HTML :: iolist().
chessman(?empty) ->
    "&nbsp;";
chessman(Chessman) ->
    "&#" ++ integer_to_list(chessman_(Chessman)) ++ ";".

%% @doc
-spec chessman_(Chessman :: echessd_game:chessman()) ->
                       Codepoint :: pos_integer().
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

%% @equiv navig_links(List, [])
%% @doc
-spec navig_links([{URL :: string(), Caption :: nonempty_string()}]) ->
                         HTML :: iolist().
navig_links(List) ->
    navig_links(List, []).

%% @doc
-spec navig_links([{URL :: string(), Caption :: nonempty_string()}],
                  Options :: [{prepend, HTML :: iolist()}]) ->
                         HTML :: iolist().
navig_links(List, Options) ->
    tag("div", ["class=navig"],
        [proplists:get_value(prepend, Options, ""),
         case List of
             [_ | _] ->
                 ["[&nbsp;",
                  string:join(
                    lists:map(
                      fun({[_ | _] = URL, [_ | _] = Caption}) ->
                              a(URL, Caption);
                         ({_, [_ | _] = Caption}) ->
                              Caption
                      end, List), "&nbsp;|&nbsp;"),
                  "&nbsp;]"];
             _ -> ""
         end]).

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
-spec game_navigation(Session :: #session{}, GameInfo :: #gameinfo{}) ->
                             HTML :: iolist().
game_navigation(Session, GameInfo) ->
    navig_links(
      case Session#session.username of
          [_ | _] ->
              [{"/", gettext(Session, txt_home, [])}] ++
                  if GameInfo#gameinfo.is_my_game andalso
                     GameInfo#gameinfo.status == ?gs_alive ->
                          [{url([{?Q_GOTO, ?SECTION_DRAW_CONFIRM},
                                 {?Q_GAME, GameInfo#gameinfo.id}]),
                            gettext(Session, txt_req_draw, [])},
                           {url([{?Q_GOTO, ?SECTION_GIVEUP_CONFIRM},
                                 {?Q_GAME, GameInfo#gameinfo.id}]),
                            gettext(Session, txt_do_giveup, [])}];
                     true -> ""
                  end ++
                  [{url([{?Q_GOTO, ?SECTION_EXIT}]),
                    gettext(Session, txt_logout, [])}];
          _ -> [{"/", gettext(Session, txt_authenticate, [])}]
      end,
      [{prepend, gettext(Session, txt_game, []) ++ " #" ++
            integer_to_list(GameInfo#gameinfo.id) ++ ": "}]).

%% @doc
-spec format_error(Session :: #session{},
                   Error :: {error, Reason :: any()} | any()) ->
                          PlainErrorMessage :: iolist().
format_error(Session, {error, Reason}) ->
    format_error(Session, Reason);
format_error(_Session, Term) ->
    io_lib:format("~120p", [Term]).

%% @doc
-spec warning(Session :: #session{}, TextID :: atom(), Args :: list()) ->
                     HTML :: iolist().
warning(Session, TextID, Args) ->
    tag("div", ["class=warning"], gettext(Session, TextID, Args)).

%% @doc
-spec gettext(Session :: #session{}, FormatTextID :: atom(),
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
