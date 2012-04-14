%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 21 Jan 2012
%%% @copyright 2012, Aleksey Morarash
%%% @doc HTML page generation functions.

-module(echessd_html).

-export([login/0,
         register/0,
         edituser/0,
         passwd/0,
         eaccess/0,
         home/0,
         game/2,
         draw_confirm/1,
         giveup_confirm/1,
         users/0,
         user/1,
         newgame/0,
         error/1, error/2,
         redirection/1,
         notyet/0
        ]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Makes 'login' page content.
%% @spec login() -> io_list()
login() ->
    Content =
        navig_links([{"?goto=" ++ ?SECTION_REG, gettext(txt_lgn_rnu_link)}]) ++
        case echessd_cfg:get(?CFG_SHOW_ABOUT) of
            true -> "<br>" ++ tag(p, gettext(txt_about)) ++ "<br>";
            _ -> ""
        end ++
        "<form method=post>"
        "<input name=action type=hidden value=login>" ++
        gettext(txt_lgn_login) ++ ": <input name=username type=text><br>" ++
        gettext(txt_lgn_passw) ++ ": <input name=password type=password><br>"
        "<input type=submit class=btn value='" ++ gettext(txt_lgn_ok_button) ++ "'>"
        "</form>" ++
        case echessd_cfg:get(?CFG_SHOW_COPYRIGHTS) of
            true -> "<hr>" ++ tag(p, gettext(txt_copyrights));
            _ -> ""
        end,
    log_reg_page("echessd - " ++ gettext(txt_lgn_title), Content).

%% @doc Makes 'register new user' page content.
%% @spec register() -> io_list()
register() ->
    Timezones =
        [echessd_lib:time_offset_to_list(O) ||
            O <- echessd_lib:administrative_offsets()],
    ServerZone =
        echessd_lib:time_offset_to_list(
          echessd_lib:local_offset()),
    DefLang = echessd_cfg:get(?CFG_DEF_LANG),
    Content =
        navig_links([{"?goto=" ++ ?SECTION_LOGIN, gettext(txt_rnu_ret_link)}]) ++
        "<form method=post>"
        "<input name=action type=hidden value=register>" ++
        gettext(txt_rnu_login) ++ ": <input name=regusername type=text><br>" ++
        gettext(txt_rnu_passw) ++ ": <input name=regpassword1 type=password><br>" ++
        gettext(txt_rnu_passw_conf) ++ ": <input name=regpassword2 type=password><br>" ++
        gettext(txt_rnu_fullname) ++ ": <input name=regfullname type=text> (" ++
        gettext(txt_rnu_optional) ++ ")<br>" ++
        gettext(txt_rnu_timezone) ++ ": <select name=regtimezone>" ++
        lists:map(
          fun(Zone) when Zone == ServerZone ->
                  "<option value='" ++ Zone ++ "' selected>" ++
                      Zone ++ "</option>";
             (Zone) ->
                  "<option value='" ++ Zone ++ "'>" ++ Zone ++ "</option>"
          end, Timezones) ++
        "</select><br>" ++
        gettext(txt_rnu_language) ++ ": <select name=reglanguage>" ++
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
        ++ gettext(txt_jid) ++ ": <input name=regjid type=text value=''> (" ++
        gettext(txt_rnu_optional) ++ ")<br>" ++
        "<label for=sil>"
        "<input type=checkbox id=sil name=regshowinlist checked>&nbsp;"
        ++ gettext(txt_rnu_show_in_list) ++ "</label><br>"
        "<input type=submit class=btn value='" ++
        gettext(txt_rnu_ok_button) ++ "'>"
        "</form>",
    log_reg_page("echessd - " ++ gettext(txt_rnu_title), Content).

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
    OldStyle = proplists:get_value(style, UserInfo),
    JID =
        echessd_lib:escape_html_entities(
          proplists:get_value(jid, UserInfo, "")),
    html_page_header(
      "echessd - " ++ gettext(txt_edit_profile_title),
      [{h1, gettext(txt_edit_profile_title)}]) ++
        navigation() ++
        "<br>" ++
        navig_links(
          [{"?goto=" ++ ?SECTION_PASSWD_FORM,
            gettext(txt_predit_passwd_link)}]) ++
        "<form method=post>"
        "<input name=action type=hidden value=" ++ ?SECTION_SAVEUSER ++ ">" ++
        gettext(txt_fullname) ++ ": <input name=editfullname type=text "
        "value='" ++ Fullname ++ "'><br>" ++
        gettext(txt_timezone) ++ ": <select name=edittimezone>" ++
        lists:map(
          fun(Zone) when Zone == Timezone ->
                  "<option value='" ++ Zone ++ "' selected>" ++
                      Zone ++ "</option>";
             (Zone) ->
                  "<option value='" ++ Zone ++ "'>" ++ Zone ++ "</option>"
          end, Timezones) ++
        "</select><br>" ++
        gettext(txt_predit_lang) ++ ": <select name=editlanguage>" ++
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
        ++ gettext(txt_style) ++ ": <select name=editstyle>" ++
        lists:map(
          fun({N, TxtID, _F}) ->
                  "<option value=" ++ atom_to_list(N) ++
                      if N == OldStyle -> " selected";
                         true -> ""
                      end ++ ">" ++ gettext(TxtID) ++ "</option>"
          end, echessd_lib:styles()) ++
        "</select><br>"
        ++ gettext(txt_jid) ++ ": <input name=editjid type=text "
        "value='" ++ JID ++ "'> (" ++
        gettext(txt_rnu_optional) ++ ")<br>" ++
        "<label for=sil>"
        "<input type=checkbox id=sil name=editshowinlist" ++
        case proplists:get_value(show_in_list, UserInfo) of
            false -> "";
            _ -> " checked"
        end ++ ">&nbsp;"
        ++ gettext(txt_rnu_show_in_list) ++ "</label><br>"
        "<label for=sh>"
        "<input type=checkbox id=sh name=editshowhistory" ++
        case proplists:get_value(show_history, UserInfo) of
            false -> "";
            _ -> " checked"
        end ++ ">&nbsp;"
        ++ gettext(txt_rnu_show_history) ++ "</label><br>"
        "<label for=sc>"
        "<input type=checkbox id=sc name=editshowcomment" ++
        case proplists:get_value(show_comment, UserInfo) of
            false -> "";
            _ -> " checked"
        end ++ ">&nbsp;"
        ++ gettext(txt_rnu_show_comment) ++ "</label><br>"
        "<input type=submit class=btn value='" ++
        gettext(txt_predit_save_button) ++ "'>"
        "</form>"
        "<br>" ++
        html_page_footer([]).

%% @doc Makes 'change user password' page content.
%% @spec passwd() -> io_list()
passwd() ->
    html_page_header(
      "echessd - " ++ gettext(txt_passwd_title),
      [{h1, gettext(txt_passwd_title)}]) ++
        navigation() ++
        "<br>" ++
        "<form method=post>"
        "<input name=action type=hidden value=" ++ ?SECTION_PASSWD ++ ">" ++
        gettext(txt_passwd_passw) ++ ": "
        "<input name=editpassword0 type=password><br>" ++
        gettext(txt_passwd_passw_new) ++ ": "
        "<input name=editpassword1 type=password><br>" ++
        gettext(txt_passwd_passw_new_confirm) ++ ": "
        "<input name=editpassword2 type=password><br>" ++
        "<input type=submit class=btn value='" ++
        gettext(txt_passwd_save_button) ++ "'>"
        "</form>"
        "<br>" ++
        html_page_footer([]).

%% @doc Makes 'home' page content.
%% @spec home() -> io_list()
home() ->
    Username = get(username),
    {ok, UserInfo} = echessd_user:getprops(Username),
    html_page_header(
      "echessd - " ++ gettext(txt_home),
      [{h1, gettext(txt_home) ++ ": " ++ Username}]) ++
        navigation() ++
        navig_links([{"?goto=" ++ ?SECTION_EDITUSER,
                      gettext(txt_edit_profile_title)}]) ++
        "<br>" ++
        user_info(Username, UserInfo) ++
        "<br>" ++
        user_games(Username, UserInfo, true) ++
        html_page_footer([]).

%% @doc Makes 'registered users list' page content.
%% @spec users() -> io_list()
users() ->
    {ok, Users0} = echessd_user:list(),
    Users = lists:usort(Users0) -- [get(username)],
    Title = gettext(txt_users),
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
                       gettext(txt_user_fetch_error) ++ ":~n~p",
                       [Username, Reason])
            end
    end.
user(Username, UserInfo) ->
    Title = gettext(txt_user) ++ " '" ++ Username ++ "'",
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
    Iam = get(username),
    H2Title =
        if Iam == Opponent ->
                gettext(txt_ng_title_h2_test, [Iam, Iam]);
           true ->
                gettext(txt_ng_title_h2_normal, [Iam, Opponent])
        end,
    ColorSelector =
        if Iam == Opponent ->
                "<input name=color type=hidden value=white>";
           true ->
                gettext(txt_ng_color) ++ ": <select name=color>"
                    "<option value='random'>" ++ gettext(txt_ng_color_random) ++ "</option>"
                    "<option value='white'>" ++ gettext(txt_ng_color_white) ++ "</option>"
                    "<option value='black'>" ++ gettext(txt_ng_color_black) ++ "</option>"
                    "</select><br>"
        end,
    html_page_header("echessd - " ++ gettext(txt_ng_title),
                     [{h1, gettext(txt_ng_title)}]) ++
        navigation() ++
        h2(H2Title) ++
        "<form method=post>"
        "<input name=action type=hidden value=" ++ ?SECTION_NEWGAME ++ ">"
        "<input name=opponent type=hidden value='" ++ Opponent ++ "'>"
        "<input name=gametype type=hidden value=classic>"
        ++ ColorSelector ++
        "<label for=prv><input name=private type=checkbox id=prv>&nbsp;" ++
        gettext(txt_ng_private) ++ "</label><br>"
        "<input type=submit class=btn value='" ++
        gettext(txt_ng_ok_button) ++ "'>"
        "</form>" ++
        html_page_footer([]).

%% @doc Makes 'game' page content.
%% @spec game(GameID, Step) -> io_list()
%%     GameID = echessd_game:echessd_game_id(),
%%     Step = integer() | last
game(GameID, Step) ->
    case fetch_game(GameID) of
        {ok, GameInfo} -> game(GameID, GameInfo, Step);
        ErrorContent -> ErrorContent
    end.
game(GameID, GameInfo, Step) ->
    Iam = get(username),
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
                "echessd: " ++ gettext(txt_your_move);
           true ->
                "echessd: " ++ gettext(txt_game) ++ " #" ++
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
    {ShowHistory, ShowComment} =
        case get(userinfo) of
            UserInfo when is_list(UserInfo) ->
                {proplists:get_value(show_history, UserInfo, true),
                 proplists:get_value(show_comment, UserInfo, true)};
            _ -> {true, true}
        end,
    ChessTable =
        chess_table(
          GameID, HistoryLen, IsLast, GameType, Board,
          IsRotated, ActiveCells, LastPly) ++
        case Comment of
            [_ | _] when ShowComment ->
                tag(p, gettext(txt_comment) ++ ": " ++ Comment);
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
                    gettext(txt_move_caption) ++ ":&nbsp;"
                    "<input name=action type=hidden value=move>"
                    "<input name=game type=hidden value=" ++
                    integer_to_list(GameID) ++ ">"
                    "<input name=move type=text size=5 id=edmv>"
                    "<input type=submit class=btn value='" ++
                    gettext(txt_move_ok_button) ++ "'>"
                    "<input type=reset class=btn value='" ++
                    gettext(txt_move_reset_button) ++ "' "
                    "onclick='clr();'><br>" ++
                    gettext(txt_move_comment_caption) ++ ":&nbsp;"
                    "<input type=text name=comment>"
                    "</form><br>";
            true -> ""
        end ++
        captures(Captures),
    html_page_header(Title, []) ++
        game_navigation(GameID, IsMyGame andalso GameStatus == none) ++
        case GameStatus of
            checkmate ->
                h2(gettext(txt_gt_over_checkmate, [userlink(Winner)]));
            give_up ->
                h2(gettext(txt_gt_over_giveup, [userlink(Winner)]));
            {draw, stalemate} ->
                h2(gettext(txt_gt_over_stalemate));
            {draw, agreement} ->
                h2(gettext(txt_gt_over_agreement));
            {draw, DrawType} ->
                h2(gettext(txt_gt_over_draw, [DrawType]));
            _ ->
                case proplists:get_value(
                       draw_request_from, GameInfo) of
                    Iam when IsMyGame ->
                        tag("div", ["class=warning"],
                            gettext(txt_gt_youre_drawing));
                    Opponent when IsMyGame ->
                        tag("div", ["class=warning"],
                            gettext(txt_gt_opponent_drawing,
                                    [userlink(Opponent)]));
                    _ -> ""
                end
        end ++
        if ShowHistory ->
                tag(table, ["cellpadding=0", "cellspacing=0", "border=0"],
                    tr(
                      [tag(td, ["valign=top"], ChessTable),
                       tag(td, ["valign=top"],
                           game_history(Step, GameID, FullHistory))]));
           true -> ChessTable
        end ++
        html_page_footer([]).

%% @doc Makes 'draw confirmation' page content.
%% @spec draw_confirm(GameID) -> io_list()
%%     GameID = echessd_game:echessd_game_id()
draw_confirm(GameID) ->
    html_page_header("echessd - " ++ gettext(txt_draw_confirm_title),
                     [{h1, gettext(txt_draw_confirm_title)}]) ++
        tag("div", ["class=warning"],
            gettext(txt_draw_confirm_text)) ++
        "<form method=post>"
        "<input type=hidden name=action value=" ++ ?SECTION_DRAW ++ ">"
        "<input type=hidden name=game value=" ++
        integer_to_list(GameID) ++ ">"
        "<input type=submit class=btn value='" ++
        gettext(txt_draw_button) ++ "'>"
        "</form>" ++
        navig_links([{"javascript: history.back();",
                      gettext(txt_ouch_back_link)}]) ++
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
    html_page_header("echessd - " ++ gettext(txt_giveup_confirm_title),
                     [{h1, gettext(txt_giveup_confirm_title)}]) ++
        tag("div", ["class=warning"],
            gettext(txt_giveup_confirm_text, [userlink(Opponent)])) ++
        "<form method=post>"
        "<input type=hidden name=action value=" ++ ?SECTION_GIVEUP ++ ">"
        "<input type=hidden name=game value=" ++
        integer_to_list(GameID) ++ ">"
        "<input type=submit class=btn value='" ++
        gettext(txt_giveup_button) ++ "'>"
        "</form>" ++
        navig_links([{"javascript: history.back();",
                      gettext(txt_ouch_back_link)}]) ++
        html_page_footer([]).

%% @doc Makes 'under construction' page content.
%% @spec notyet() -> io_list()
notyet() ->
    html_page_header("echessd - " ++ gettext(txt_not_implemented_title),
                     [{h1, gettext(txt_not_implemented_title)}]) ++
        h2(gettext(txt_not_implemented_text)) ++
        navig_links([{"javascript: history.back();", "Back"}]) ++
        html_page_footer([]).

%% @doc Makes 'error' page content.
%% @spec error(Message) -> io_list()
%%     Message = io_list()
error(Message) ->
    html_page_header("echessd - " ++ gettext(txt_error_page_title),
                     [{h1, gettext(txt_error_page_title)}]) ++
        tag("div", ["class=error"], pre(Message)) ++
        "<br>" ++
        navig_links([{"javascript: history.back();", gettext(txt_back_link)}]) ++
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
    ?MODULE:error(gettext(txt_access_denied)).

%% @doc Makes 303-redirection page content.
%% @spec redirection(URL) -> io_list()
%%     URL = string()
redirection(URL) ->
    html_page_header("echessd - " ++ gettext(txt_redirection),
                     [{h1, gettext(txt_redirection)}]) ++
        tag(p, gettext(txt_redirection_description)) ++
        a(URL, URL) ++
        html_page_footer([]).

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

js_init(Grouped, ActiveCells, LastPly) ->
    "var ACs = [" ++
        string:join(["'" ++ C ++ "'" || C <- ActiveCells], ",")
        ++ "];\n"
        "var I1s = [" ++
        string:join(["'" ++ I1 ++ "'" || {I1, _} <- Grouped], ",") ++ "];\n"
        "var I2s = [" ++
        string:join(
          ["[" ++
               string:join(["'" ++ I2 ++ "'" || I2 <- L], ",")
           ++ "]" || {_, L} <- Grouped], ",")
        ++ "];\n"
        "var LPs = [" ++
        case LastPly of
            [A, B, C, D] ->
                "'" ++ [A, B] ++ "','" ++ [C, D] ++ "'";
            _ -> ""
        end ++ "];\n".

group_possibles([], Result) -> Result;
group_possibles([[A, B, C, D] | Tail], [{[A, B], List} | ResTail]) ->
    group_possibles(Tail, [{[A, B], [[C, D] | List]} | ResTail]);
group_possibles([[A, B, C, D] | Tail], Result) ->
    group_possibles(Tail, [{[A, B], [[C, D]]} | Result]).

log_reg_page(Title, Content) ->
    html_page_header(Title, []) ++
        navig_links(
          lists:map(
            fun(Lang) ->
                    Section =
                        case get(section) of
                            [_ | _] = Section0 -> Section0;
                            _ -> ?SECTION_LOGIN
                        end,
                    {"/?goto=" ++ Section ++ "&lang=" ++ Lang, Lang}
            end,
            [atom_to_list(A) || {A, _} <- echessd_lib:languages()])) ++
        h1(Title) ++
        Content ++
        html_page_footer([]).

fetch_game(GameID) ->
    case echessd_game:getprops(GameID) of
        {ok, GameInfo} ->
            case proplists:get_value(private, GameInfo) of
                true ->
                    case is_my_game(GameInfo) of
                        true -> {ok, GameInfo};
                        _ ->
                            Reason = {no_such_game, GameID},
                            ?MODULE:error(
                               gettext(txt_game_fetch_error) ++ ":~n~p",
                               [GameID, Reason])
                    end;
                _ ->
                    case proplists:get_value(
                           acknowledged, GameInfo) of
                        true -> {ok, GameInfo};
                        _ ->
                            ?MODULE:error(
                               gettext(
                                 txt_game_not_confirmed_error,
                                 [GameID]))
                    end
            end;
        {error, Reason} ->
            ?MODULE:error(
               gettext(txt_game_fetch_error) ++ ":~n~p",
               [GameID, Reason])
    end.

is_my_game(GameInfo) ->
    Iam = get(username),
    Users = proplists:get_value(users, GameInfo, []),
    case [N || {N, C} <- Users, N == Iam,
               lists:member(C, [?white, ?black])] of
        [_ | _] -> true;
        _ -> false
    end.

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
      fun(login) -> [{gettext(txt_login), Username}];
         (fullname = Key) ->
              [{gettext(txt_fullname),
                case proplists:get_value(Key, UserInfo) of
                    [_ | _] = Value ->
                        echessd_lib:escape_html_entities(Value);
                    _ -> gettext(txt_not_sure)
                end}];
         (created = Key) ->
              [{gettext(txt_registered),
                case proplists:get_value(Key, UserInfo) of
                    Value when ?is_now(Value) ->
                        echessd_lib:timestamp(
                          Value, get(timezone));
                    _ -> gettext(txt_unknown)
                end}];
         (timezone = Key) ->
              [{gettext(txt_timezone),
                case proplists:get_value(Key, UserInfo) of
                    Value when is_tuple(Value) ->
                        echessd_lib:time_offset_to_list(Value);
                    _ ->
                        echessd_lib:time_offset_to_list(
                          echessd_lib:local_offset())
                end}];
         (language) ->
              {_LangAbbr, LangName} = LangInfo,
              [{gettext(txt_language), LangName}];
         (_) -> []
      end, [login, fullname, created, timezone, language]).

user_games(Username, UserInfo, ShowNotAcknowledged) ->
    %% fetch all user games info
    UserGames =
        lists:flatmap(
          fun(GameID) ->
                  case echessd_game:getprops(GameID) of
                      {ok, GameInfo} ->
                          Visible =
                              not (proplists:get_value(private, GameInfo) == true)
                              orelse is_my_game(GameInfo),
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
          end, proplists:get_value(games, UserInfo, [])),
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
            h2(gettext(txt_user_games) ++ ":") ++
                string:join(
                  [user_game_(Username, I, L) ||
                      {I, L} <- NotEnded], "<br>") ++
                "<br>";
        _ -> ""
    end ++
        case NotConfirmed of
            [_ | _] when ShowNotAcknowledged ->
                h2(gettext(txt_unconf_games) ++ ":") ++
                    string:join(
                      [user_unconfirmed_game_(Username, I, L) ||
                          {I, L} <- NotConfirmed], "<br>") ++
                    "<br>";
            _ -> ""
        end ++
        case Ended of
            [_ | _] ->
                h2(gettext(txt_user_ended_games) ++ ":") ++
                    string:join(
                      [user_game_(Username, I, L) ||
                          {I, L} <- Ended], "<br>") ++
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
        if IsTest -> " " ++ gettext(txt_test_game);
           true ->
                Color = proplists:get_value(Owner, GamePlayers),
                Opponent = hd(UniquePlayerNames -- [Owner]),
                OpponentColor = proplists:get_value(Opponent, GamePlayers),
                " " ++ chessman({Color, ?king}) ++ " " ++ gettext(txt_vs) ++
                    " " ++ userlink(Opponent) ++ " " ++
                    chessman({OpponentColor, ?king})
        end ++
        case proplists:get_value(status, GameInfo) of
            none ->
                case get(username) == echessd_game:who_must_turn(GameInfo) of
                    true when not IsTest -> " !!!";
                    _ -> ""
                end;
            checkmate when IsTest -> " - " ++ gettext(txt_checkmate);
            checkmate ->
                case proplists:get_value(winner, GameInfo) of
                    Owner -> " - " ++ gettext(txt_win);
                    _ -> " - " ++ gettext(txt_loose)
                end;
            give_up when IsTest -> " - " ++ gettext(txt_gived_up);
            give_up ->
                case proplists:get_value(winner, GameInfo) of
                    Owner -> " - " ++ gettext(txt_win_giveup);
                    _ -> " - " ++ gettext(txt_loose_giveup)
                end;
            {draw, _} ->
                " - " ++ gettext(txt_draw)
        end.

user_unconfirmed_game_(Owner, GameID, GameInfo) ->
    StrGameID = integer_to_list(GameID),
    GamePlayers =
        [{N, C} || {N, C} <- proplists:get_value(users, GameInfo, []),
                   lists:member(C, [?white, ?black])],
    UniquePlayerNames = lists:usort([N || {N, _} <- GamePlayers]),
    "* #" ++ StrGameID ++ " " ++
        case proplists:get_value(creator, GameInfo) of
            Owner ->
                Opponent = hd(UniquePlayerNames -- [Owner]),
                OpponentColor = proplists:get_value(Opponent, GamePlayers),
                gettext(txt_waiting_for,
                        [userlink(Opponent) ++ " " ++
                             chessman({OpponentColor, ?king})]);
            Opponent ->
                OpponentColor = proplists:get_value(Opponent, GamePlayers),
                AckURL =
                    "?action=" ++ ?SECTION_ACKGAME ++ "&game=" ++ StrGameID,
                gettext(txt_waiting_for_you,
                        [userlink(Opponent) ++ " " ++
                             chessman({OpponentColor, ?king})]) ++ " " ++
                    tag("a", ["href='" ++ AckURL ++ "'"],
                        gettext(txt_game_confirm))
        end ++ " " ++
        tag("a", ["href='" ++ "?action=" ++ ?SECTION_DENYGAME ++
                      "&game=" ++ StrGameID ++"'"],
            gettext(txt_game_deny)).

html_page_header(Title, Options) ->
    UserStyle =
        case get(userinfo) of
            UserInfo when is_list(UserInfo) ->
                proplists:get_value(style, UserInfo);
            _ -> undefined
        end,
    StylesFilename =
        case [F || {N, _T, F} <- echessd_lib:styles(), N == UserStyle] of
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
                    pre(gettext(txt_error) ++ ": " ++ format_error(Error)))
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
tag(Tag, Attrs, Value) when is_atom(Tag) ->
    tag(atom_to_list(Tag), Attrs, Value);
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
        gettext(txt_new_game_link)}]).

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

cell(Board, C, R) -> element(C, element(8 - R + 1, Board)).

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
ghc(N, StrW, StrB) ->
    tr(
      [tag(td, ["valign=bottom"], tt(integer_to_list(N) ++ ".&nbsp;")),
       tag(td, ["valign=bottom"], StrW), td("&nbsp;"),
       tag(td, ["valign=bottom"], StrB)]).
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

cseq(1) -> lists:seq(1, 8);
cseq(-1) -> lists:seq(8, 1, -1).

inply([A, B, _C, _D | _], [A, B]) -> true;
inply([_A, _B, C, D | _], [C, D]) -> true;
inply(_, _) -> false.

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

navig_links(List) ->
    navig_links(List, []).
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

navigation() ->
    case get(username) of
        [_ | _] ->
            navig_links(
              [{"/", gettext(txt_home)},
               {"/?goto=" ++ ?SECTION_USERS, gettext(txt_users)},
               {"/?action=" ++ ?SECTION_EXIT, gettext(txt_logout)}]);
        _ -> ""
    end.

game_navigation(GameID, ShowEndGameLinks) ->
    StrID = integer_to_list(GameID),
    Links =
        [{"/?goto=" ++ ?SECTION_DRAW_CONFIRM ++
              "&game=" ++ StrID, gettext(txt_req_draw)},
         {"/?goto=" ++ ?SECTION_GIVEUP_CONFIRM ++
              "&game=" ++ StrID, gettext(txt_do_giveup)}],
    navig_links(
      case get(username) of
          [_ | _] ->
              [{"/", gettext(txt_home)}] ++
                  if ShowEndGameLinks -> Links;
                     true -> ""
                  end ++
                  [{"/?action=" ++ ?SECTION_EXIT, gettext(txt_logout)}];
          _ -> [{"/", gettext(txt_authenticate)}]
      end,
      [{prepend, gettext(txt_game) ++ " #" ++
            integer_to_list(GameID) ++ ": "}]).

format_error({error, Reason}) ->
    format_error(Reason);
format_error({wrong_move, Reason}) ->
    gettext(txt_badmove) ++
        case Reason of
            check -> gettext(txt_king_in_check);
            badmove -> "";
            friendly_fire -> "";
            _ -> format_error(Reason)
        end;
format_error(Term) ->
    io_lib:format("~120p", [Term]).

gettext(TextID) ->
    echessd_lib:gettext(TextID, get(language)).
gettext(TextID, Args) ->
    io_lib:format(gettext(TextID), Args).

