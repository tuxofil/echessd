%%% @doc
%%% HTTP request processing functions.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 2 Feb 2012
%%% @copyright 2012, Aleksey Morarash

-module(echessd_request_processor).

-export([handle/4]).

-include("echessd.hrl").

-define(is_logged_in(Session), (Session#session.username /= undefined)).

%% ----------------------------------------------------------------------
%% Type definitions
%% ----------------------------------------------------------------------

-export_type([result/0]).

-type result() ::
        {redirect, URL :: string()} |
        (HtmlPageContent :: iolist()).

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Handle HTTP request and return HTML page contents.
-spec handle(Method :: nonempty_string(),
             Section :: echessd_query_parser:section(),
             Query :: echessd_query_parser:http_query(),
             Session :: #session{}) -> result().
handle(?HTTP_GET, Section, Query, Session) ->
    handle_get(Section, Query, Session);
handle(?HTTP_POST, Section, Query, Session) ->
    handle_post(Section, Query, Session).

%% @doc Handle HTTP GET request and return HTML page contents.
-spec handle_get(Section :: echessd_query_parser:section(),
                 Query :: echessd_query_parser:http_query(),
                 Session :: #session{}) -> result().
handle_get(?SECTION_EXIT, _Query, Session)
  when ?is_logged_in(Session) ->
    ok = echessd_session:del(Session),
    {redirect, "/"};
handle_get(?SECTION_ACKGAME, Query, Session)
  when ?is_logged_in(Session) ->
    GameID = proplists:get_value(?Q_GAME, Query),
    case echessd_game:ack(GameID, Session#session.username) of
        ok ->
            redirect_to_game(GameID);
        {error, Reason} ->
            geterr(Session, txt_err_game_confirm, Reason)
    end;
handle_get(?SECTION_DENYGAME, Query, Session)
  when ?is_logged_in(Session) ->
    GameID = proplists:get_value(?Q_GAME, Query),
    case echessd_game:deny(GameID, Session#session.username) of
        ok ->
            {redirect, "/"};
        {error, Reason} ->
            geterr(Session, txt_err_game_deny, Reason)
    end;
handle_get(_, Query, Session)
  when ?is_logged_in(Session) ->
    handle_show(Session, Query);
handle_get(Section, Query, Session) ->
    NewSession =
        case proplists:get_value(?Q_LANG, Query) of
            undefined ->
                Session;
            LangID ->
                echessd_httpd_lib:add_extra_headers(
                  [{"Set-Cookie", "lang=" ++ atom_to_list(LangID)}]),
                Session#session{language = LangID}
        end,
    case Section of
        ?SECTION_REG ->
            echessd_html:register(NewSession);
        ?SECTION_GAME ->
            handle_show(NewSession, Query);
        _ ->
            echessd_html:login(NewSession)
    end.

%% @doc Handle HTTP POST request and return HTML page contents.
-spec handle_post(Section :: echessd_query_parser:section(),
                  Query :: echessd_query_parser:http_query(),
                  Session :: #session{}) -> result().
handle_post(?SECTION_LOGIN, Query, Session) ->
    Username = proplists:get_value(?Q_USERNAME, Query),
    Password = proplists:get_value(?Q_PASSWORD, Query),
    case ?is_logged_in(Session) andalso
        Session#session.username == Username of
        true ->
            handle_show(Session, Query);
        _ ->
            case echessd_user:auth(Username, Password) of
                {ok, _UserInfo} ->
                    ok = echessd_session:save(
                           Session#session{username = Username}),
                    NewSession = echessd_session:get(Session#session.id),
                    SID = NewSession#session.id,
                    echessd_log:debug(
                      "session ~9999p created for user ~9999p",
                      [SID, Username]),
                    StrLangID = atom_to_list(NewSession#session.language),
                    echessd_httpd_lib:add_extra_headers(
                      [{"Set-Cookie", "sid=" ++ SID},
                       {"Set-Cookie", "lang=" ++ StrLangID}]),
                    {redirect, "/"};
                _ ->
                    echessd_html:eaccess(Session)
            end
    end;
handle_post(?SECTION_REG, Query, Session)
  when not ?is_logged_in(Session) ->
    Username = proplists:get_value(?Q_EDIT_USERNAME, Query),
    Fullname = proplists:get_value(?Q_EDIT_FULLNAME, Query),
    Password1 = proplists:get_value(?Q_EDIT_PASSWORD1, Query),
    Password2 = proplists:get_value(?Q_EDIT_PASSWORD2, Query),
    Timezone = proplists:get_value(?Q_EDIT_TIMEZONE, Query),
    Language = proplists:get_value(?Q_EDIT_LANGUAGE, Query),
    ShowInList = proplists:is_defined(?Q_EDIT_SHOW_IN_LIST, Query),
    JID = proplists:get_value(?Q_EDIT_JID, Query),
    if Password1 /= Password2 ->
            geterr(Session, txt_passw_conf_error);
       true ->
            case echessd_user:add(
                   Username,
                   [{?ui_password, Password1},
                    {?ui_jid, JID},
                    {?ui_fullname, Fullname},
                    {?ui_timezone, Timezone},
                    {?ui_language, Language},
                    {?ui_show_in_list, ShowInList},
                    {?ui_created, now()}]) of
                ok ->
                    handle_post(
                      ?SECTION_LOGIN,
                      [{?Q_USERNAME, Username}, {?Q_PASSWORD, Password1}],
                      Session);
                {error, Reason} ->
                    geterr(Session, txt_err_new_user, Reason)
            end
    end;
handle_post(?SECTION_PASSWD, Query, Session)
  when ?is_logged_in(Session) ->
    Password0 = proplists:get_value(?Q_EDIT_PASSWORD0, Query),
    Password1 = proplists:get_value(?Q_EDIT_PASSWORD1, Query),
    Password2 = proplists:get_value(?Q_EDIT_PASSWORD2, Query),
    case echessd_user:auth(Session#session.username, Password0) of
        {ok, _UserInfo} when Password1 == Password2 ->
            case echessd_user:passwd(Session#session.username, Password1) of
                ok ->
                    {redirect, "/"};
                {error, Reason} ->
                    geterr(Session, txt_err_passwd, Reason)
            end;
        {ok, _UserInfo} ->
            geterr(Session, txt_passw_conf_error);
        {error, _Reason} ->
            echessd_html:eaccess(Session)
    end;
handle_post(?SECTION_SAVEUSER, Query, Session)
  when ?is_logged_in(Session) ->
    Fullname = proplists:get_value(?Q_EDIT_FULLNAME, Query),
    Timezone = proplists:get_value(?Q_EDIT_TIMEZONE, Query),
    Language = proplists:get_value(?Q_EDIT_LANGUAGE, Query),
    ShowInList = proplists:is_defined(?Q_EDIT_SHOW_IN_LIST, Query),
    ShowHistory = proplists:is_defined(?Q_EDIT_SHOW_HISTORY, Query),
    ShowComment = proplists:is_defined(?Q_EDIT_SHOW_COMMENT, Query),
    Notify = proplists:is_defined(?Q_EDIT_NOTIFY, Query),
    AutoRefresh = proplists:is_defined(?Q_EDIT_AUTO_REFRESH, Query),
    AutoRefreshPeriod = proplists:get_value(?Q_EDIT_AUTO_PERIOD, Query),
    StyleID = proplists:get_value(?Q_EDIT_STYLE, Query),
    JID = proplists:get_value(?Q_EDIT_JID, Query),
    NewUserInfo =
        [{?ui_fullname, Fullname},
         {?ui_timezone, Timezone},
         {?ui_language, Language},
         {?ui_style, StyleID},
         {?ui_jid, JID},
         {?ui_notify, Notify},
         {?ui_auto_refresh, AutoRefresh},
         {?ui_auto_refresh_period, AutoRefreshPeriod},
         {?ui_show_history, ShowHistory},
         {?ui_show_comment, ShowComment},
         {?ui_show_in_list, ShowInList}],
    case echessd_user:setprops(Session#session.username, NewUserInfo) of
        ok ->
            {redirect, "/"};
        {error, Reason} ->
            geterr(Session, txt_err_save_user, Reason)
    end;
handle_post(?SECTION_NEWGAME, Query, Session)
  when ?is_logged_in(Session) ->
    Opponent = proplists:get_value(?Q_OPPONENT, Query),
    Color = proplists:get_value(?Q_COLOR, Query),
    GameType = proplists:get_value(?Q_GAMETYPE, Query),
    Private = proplists:is_defined(?Q_PRIVATE, Query),
    case echessd_game:add(
           GameType, Session#session.username, Color, Opponent,
           [{?gi_private, Private}]) of
        {ok, GameID} when Session#session.username == Opponent ->
            redirect_to_game(GameID);
        {ok, _GameID} ->
            {redirect, "/"};
        {error, Reason} ->
            geterr(Session, txt_err_new_game, Reason)
    end;
handle_post(?SECTION_MOVE, Query, Session)
  when ?is_logged_in(Session) ->
    GameID = proplists:get_value(?Q_GAME, Query),
    Coords = proplists:get_value(?Q_MOVE, Query),
    Comment = proplists:get_value(?Q_COMMENT, Query),
    case Coords of
        [_ | _] ->
            Ply =
                {Coords,
                 case Comment of
                     [_ | _] -> [{?pi_comment, Comment}];
                     _ -> []
                 end ++ []},
            case echessd_game:ply(
                   GameID, Session#session.username, Ply) of
                ok ->
                    nop;
                {error, ?e_not_your_turn} ->
                    %% silently ignore this
                    %% (maybe caused by old page update)
                    nop;
                Error ->
                    put(error, Error)
            end;
        _ ->
            nop
    end,
    redirect_to_game(GameID);
handle_post(?SECTION_DRAW, Query, Session)
  when ?is_logged_in(Session) ->
    GameID = proplists:get_value(?Q_GAME, Query),
    case echessd_game:request_draw(GameID, Session#session.username) of
        ok ->
            nop;
        Error ->
            put(error, Error)
    end,
    redirect_to_game(GameID);
handle_post(?SECTION_GIVEUP, Query, Session)
  when ?is_logged_in(Session) ->
    GameID = proplists:get_value(?Q_GAME, Query),
    case echessd_game:give_up(GameID, Session#session.username) of
        ok ->
            nop;
        Error ->
            put(error, Error)
    end,
    redirect_to_game(GameID);
handle_post(_, _, Session) ->
    echessd_html:eaccess(Session).

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc
-spec handle_show(Session :: #session{},
                  Query :: echessd_query_parser:http_query()) ->
                         HTML :: iolist().
handle_show(Session, Query) ->
    case proplists:get_value(?Q_GOTO, Query, ?SECTION_HOME) of
        ?SECTION_GAME ->
            echessd_html:game(Session, Query);
        ?SECTION_USERS ->
            echessd_html:users(Session);
        ?SECTION_USER ->
            echessd_html:user(Session, Query);
        ?SECTION_EDITUSER ->
            echessd_html:edituser(Session);
        ?SECTION_PASSWD_FORM ->
            echessd_html:passwd(Session);
        ?SECTION_NEWGAME ->
            echessd_html:newgame(Session, Query);
        ?SECTION_DRAW_CONFIRM ->
            echessd_html:draw_confirm(Session, Query);
        ?SECTION_GIVEUP_CONFIRM ->
            echessd_html:giveup_confirm(Session, Query);
        ?SECTION_HOME ->
            echessd_html:home(Session)
    end.

%% @doc
-spec redirect_to_game(GameID :: pos_integer()) ->
                              {redirect, URL :: nonempty_string()}.
redirect_to_game(GameID) ->
    {redirect,
     echessd_query_parser:encode(
       [{?Q_GOTO, ?SECTION_GAME}, {?Q_GAME, GameID}])}.

%% @doc
-spec gettext(Session :: #session{}, TextID :: atom()) -> string().
gettext(Session, TextID) ->
    echessd_lang:gettext(TextID, Session#session.language).

%% @doc Return formatted and localized error message.
-spec geterr(Session :: #session{}, TextID :: atom()) -> iolist().
geterr(Session, TextID) ->
    echessd_html:error(Session, gettext(Session, TextID)).

%% @doc Return formatted and localized error message.
-spec geterr(Session :: #session{}, TextID :: atom(), Reason :: any()) ->
                    iolist().
geterr(Session, TextID, Reason) ->
    echessd_html:error(
      Session, gettext(Session, TextID) ++ ":~n~9999p", [Reason]).

