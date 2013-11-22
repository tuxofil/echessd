%%% @doc
%%% HTTP request processing functions.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 2 Feb 2012
%%% @copyright 2012, Aleksey Morarash

-module(echessd_request_processor).

-export([handle_get/3, handle_post/3]).

-include("echessd.hrl").

-define(is_logged_in(Session), Session#session.username /= undefined).

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Handle HTTP GET request and return HTML page contents.
-spec handle_get(Section :: echessd_httpd:section(),
                 Query :: echessd_httpd:http_query(),
                 Session :: #session{}) -> echessd_httpd:result().
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
    handle_show(Query);
handle_get(_, Query, _Session) ->
    case echessd_lang:parse(proplists:get_value("lang", Query)) of
        {LangID, _} ->
            echessd_httpd_lib:add_extra_headers(
              [{"Set-Cookie", "lang=" ++ atom_to_list(LangID)}]),
            put(language, LangID);
        _ ->
            nop
    end,
    case get_section(Query) of
        ?SECTION_REG ->
            put(section, ?SECTION_REG),
            echessd_html:register();
        ?SECTION_GAME ->
            handle_show(?SECTION_GAME, Query);
        _ ->
            put(section, ?SECTION_LOGIN),
            echessd_html:login()
    end.

%% @doc Handle HTTP POST request and return HTML page contents.
-spec handle_post(Section :: echessd_httpd:section(),
                  Query :: echessd_httpd:http_query(),
                  Session :: #session{}) -> echessd_httpd:result().
handle_post(?SECTION_LOGIN, Query, Session) ->
    Username = proplists:get_value(?Q_USERNAME, Query),
    Password = proplists:get_value(?Q_PASSWORD, Query),
    case ?is_logged_in(Session) andalso
        Session#session.username == Username of
        true ->
            handle_show(Query);
        _ ->
            ok = echessd_session:del(Session),
            case echessd_user:auth(Username, Password) of
                {ok, _UserInfo} ->
                    SID = echessd_session:mk(Username),
                    echessd_session:read([{"sid", SID}]),
                    echessd_log:debug(
                      "session ~9999p created for user ~9999p",
                      [SID, Username]),
                    StrLangID = atom_to_list(Session#session.language),
                    echessd_httpd_lib:add_extra_headers(
                      [{"Set-Cookie", "sid=" ++ SID},
                       {"Set-Cookie", "lang=" ++ StrLangID}]),
                    {redirect, "/"};
                _ ->
                    echessd_html:eaccess()
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
                   [{password, Password1},
                    {jid, JID},
                    {fullname, Fullname},
                    {timezone, Timezone},
                    {language, Language},
                    {show_in_list, ShowInList},
                    {created, now()}]) of
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
        {ok, _UserInfo} ->
            if Password1 /= Password2 ->
                    geterr(Session, txt_passw_conf_error);
               true ->
                    NewUserInfo = [{password, Password1}],
                    case echessd_user:setprops(
                           Session#session.username, NewUserInfo) of
                        ok ->
                            {redirect, "/"};
                        {error, Reason} ->
                            geterr(Session, txt_err_passwd, Reason)
                    end
            end;
        _ ->
            echessd_html:eaccess()
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
        [{fullname, Fullname},
         {timezone, Timezone},
         {language, Language},
         {style, StyleID},
         {jid, JID},
         {notify, Notify},
         {auto_refresh, AutoRefresh},
         {auto_refresh_period, AutoRefreshPeriod},
         {show_history, ShowHistory},
         {show_comment, ShowComment},
         {show_in_list, ShowInList}],
    case echessd_user:setprops(
           Session#session.username, NewUserInfo) of
        ok ->
            echessd_session:read([{"sid", Session#session.id}]),
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
           [{private, Private}]) of
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
                     [_ | _] -> [{comment, Comment}];
                     _ -> []
                 end ++ []},
            case echessd_game:ply(
                   GameID, Session#session.username, Ply) of
                ok ->
                    nop;
                {error, not_your_turn} ->
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
handle_post(_, _, _) ->
    echessd_html:eaccess().

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc
-spec handle_show(Query :: echessd_httpd:http_query()) ->
                         echessd_httpd:result().
handle_show(Query) ->
    handle_show(proplists:get_value(?Q_GOTO, Query), Query).

%% @doc
-spec handle_show(SectionID :: echessd_httpd:section(),
                  Query :: echessd_httpd:http_query()) ->
                         echessd_httpd:result().
handle_show(?SECTION_GAME, Query) ->
    echessd_html:game(
      proplists:get_value(?Q_GAME, Query),
      proplists:get_value(?Q_STEP, Query));
handle_show(?SECTION_USERS, _Query) ->
    echessd_html:users();
handle_show(?SECTION_USER, Query) ->
    echessd_html:user(proplists:get_value(?Q_NAME, Query));
handle_show(?SECTION_EDITUSER, _Query) ->
    echessd_html:edituser();
handle_show(?SECTION_PASSWD_FORM, _Query) ->
    echessd_html:passwd();
handle_show(?SECTION_NEWGAME, _Query) ->
    echessd_html:newgame();
handle_show(?SECTION_DRAW_CONFIRM, Query) ->
    echessd_html:draw_confirm(proplists:get_value(?Q_GAME, Query));
handle_show(?SECTION_GIVEUP_CONFIRM, Query) ->
    echessd_html:giveup_confirm(proplists:get_value(?Q_GAME, Query));
handle_show(_Default, _Query) ->
    echessd_html:home().

%% @doc Fetch section ID from the query.
-spec get_section(Query :: echessd_httpd:http_query()) ->
                         Section :: echessd_httpd:section().
get_section(Query) ->
    proplists:get_value(?Q_GOTO, Query, ?SECTION_HOME).

%% @doc
-spec redirect_to_game(GameID :: pos_integer()) -> nonempty_string().
redirect_to_game(GameID) ->
    {redirect,
     echessd_httpd:encode_query(
       [{?Q_GOTO, ?SECTION_GAME}, {?Q_GAME, GameID}])}.

%% @doc
-spec gettext(Session :: #session{}, TextID :: atom()) -> string().
gettext(Session, TextID) ->
    echessd_lang:gettext(TextID, Session#session.language).

%% @doc Return formatted and localized error message.
-spec geterr(Session :: #session{}, TextID :: atom()) -> iolist().
geterr(Session, TextID) ->
    echessd_html:error(gettext(Session, TextID)).

%% @doc Return formatted and localized error message.
-spec geterr(Session :: #session{}, TextID :: atom(), Reason :: any()) ->
                    iolist().
geterr(Session, TextID, Reason) ->
    echessd_html:error(
      gettext(Session, TextID) ++ ":~n~9999p", [Reason]).

