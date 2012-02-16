%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 2 Feb 2012
%%% @copyright 2012, Aleksey Morarash
%%% @doc HTTP request processing functions

-module(echessd_request_processor).

-export([process_get/1, process_post/1]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Handles HTTP GET request and return HTML page contents.
%% @spec process_get(Query) -> HtmlPageContent
%%     Query = [{Key, Value}],
%%     Key = string(),
%%     Value = string(),
%%     HtmlPageContent = io_list()
process_get(Query) ->
    put(query_proplist, Query),
    echessd_log:debug("GET query=~9999p", [Query]),
    Action = proplists:get_value("action", Query),
    process_get(Action, Query, get(logged_in)).

%% @doc Handles HTTP POST request and return HTML page contents.
%% @spec process_post(Query) -> HtmlPageContent
%%     Query = [{Key, Value}],
%%     Key = string(),
%%     Value = string(),
%%     HtmlPageContent = io_list()
process_post(Query) ->
    put(query_proplist, Query),
    echessd_log:debug("POST query=~9999p", [Query]),
    Action = proplists:get_value("action", Query),
    process_post(Action, Query, get(logged_in)).

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

process_get(?SECTION_EXIT, _Query, true) ->
    echessd_session:del(get(sid)),
    echessd_html:login();
process_get(?SECTION_ACKGAME, _Query, true) ->
    GameID = list_to_integer(get_query_item("game")),
    case echessd_game:ack(GameID, get(username)) of
        ok ->
            process_get(
              ?SECTION_GAME,
              [{"goto", ?SECTION_GAME},
               {"game", integer_to_list(GameID)}], true);
        {error, Reason} ->
            echessd_html:error(
              gettext(txt_err_game_confirm) ++ ":~n~9999p",
              [GameID, Reason])
    end;
process_get(?SECTION_DENYGAME, _Query, true) ->
    GameID = list_to_integer(get_query_item("game")),
    case echessd_game:deny(GameID, get(username)) of
        ok ->
            process_get(
              ?SECTION_HOME,
              [{"goto", ?SECTION_HOME}], true);
        {error, Reason} ->
            echessd_html:error(
              gettext(txt_err_game_deny) ++ ":~n~9999p",
              [GameID, Reason])
    end;
process_get(_, Query, true) ->
    process_goto(Query),
    put(query_proplist, Query),
    process_show();
process_get(_, Query, _) ->
    case proplists:get_value("lang", Query) of
        [_ | _] = Lang0 ->
            case echessd_lib:parse_language(Lang0) of
                {Abbr, _} ->
                    echessd_httpd_lib:add_extra_headers(
                      [{"Set-Cookie",
                        "lang=" ++ atom_to_list(Abbr)}]),
                    put(language, Abbr);
                _ -> nop
            end;
        _ -> nop
    end,
    case proplists:get_value("goto", Query) of
        ?SECTION_REG ->
            put(section, ?SECTION_REG),
            echessd_html:register();
        ?SECTION_GAME ->
            echessd_html:game(
              list_to_integer(get_query_item("game")));
        ?SECTION_HISTORY ->
            echessd_html:history(
              list_to_integer(get_query_item("game")));
        _ ->
            put(section, ?SECTION_LOGIN),
            echessd_html:login()
    end.

process_post(?SECTION_LOGIN, Query, LoggedIn) ->
    Username = proplists:get_value("username", Query),
    Password = proplists:get_value("password", Query),
    case LoggedIn andalso get(username) == Username of
        true ->
            process_show();
        _ ->
            ok = echessd_session:del(get(sid)),
            case echessd_user:auth(Username, Password) of
                {ok, UserInfo} ->
                    SID = echessd_session:mk(Username),
                    echessd_session:read([{"sid", SID}]),
                    echessd_session:set_val(section, ?SECTION_HOME),
                    echessd_session:set_val(userinfo, UserInfo),
                    echessd_log:debug(
                      "session ~9999p created for user ~9999p",
                      [SID, Username]),
                    echessd_httpd_lib:add_extra_headers(
                      [{"Set-Cookie", "sid=" ++ SID},
                       {"Set-Cookie",
                        "lang=" ++ atom_to_list(get(language))}]),
                    process_show();
                _ ->
                    echessd_html:eaccess()
            end
    end;
process_post(?SECTION_REG, Query, false) ->
    Username = proplists:get_value("regusername", Query),
    Fullname = proplists:get_value("regfullname", Query),
    Password1 = proplists:get_value("regpassword1", Query),
    Password2 = proplists:get_value("regpassword2", Query),
    StrTimezone = proplists:get_value("regtimezone", Query),
    StrLanguage = proplists:get_value("reglanguage", Query),
    ShowInList =
        case proplists:get_value("regshowinlist", Query) of
            [_ | _] -> true;
            _ -> false
        end,
    if Password1 /= Password2 ->
            echessd_html:error(gettext(txt_passw_conf_error));
       true ->
            case echessd_lib:list_to_time_offset(StrTimezone) of
                {ok, Timezone} ->
                    case echessd_user:add(
                           Username,
                           [{password, Password1},
                            {fullname, Fullname},
                            {timezone, Timezone},
                            {language, StrLanguage},
                            {show_in_list, ShowInList},
                            {created, now()}]) of
                        ok ->
                            process_post(
                              ?SECTION_LOGIN,
                              [{"username", Username},
                               {"password", Password1}], false);
                        {error, Reason} ->
                            echessd_html:error(
                              gettext(txt_err_new_user) ++ ":~n~9999p",
                              [Reason])
                    end;
                _ ->
                    echessd_html:error(
                      gettext(txt_err_new_user) ++ ":~n" ++
                          gettext(txt_err_bad_timezone), [])
            end
    end;
process_post(?SECTION_PASSWD, Query, true) ->
    Username = get(username),
    Password0 = proplists:get_value("editpassword0", Query),
    Password1 = proplists:get_value("editpassword1", Query),
    Password2 = proplists:get_value("editpassword2", Query),
    case echessd_user:auth(Username, Password0) of
        {ok, _UserInfo} ->
            if Password1 /= Password2 ->
                    echessd_html:error(gettext(txt_passw_conf_error));
               true ->
                    NewUserInfo = [{password, Password1}],
                    case echessd_user:setprops(
                           Username, NewUserInfo) of
                        ok ->
                            process_get(
                              ?SECTION_HOME,
                              [{"goto", ?SECTION_HOME}], true);
                        {error, Reason} ->
                            echessd_html:error(
                              gettext(txt_err_passwd) ++ ":~n~9999p",
                              [Reason])
                    end
            end;
        _ ->
            echessd_html:eaccess()
    end;
process_post(?SECTION_SAVEUSER, Query, true) ->
    Username = get(username),
    Fullname = proplists:get_value("editfullname", Query),
    StrTimezone = proplists:get_value("edittimezone", Query),
    StrLanguage = proplists:get_value("editlanguage", Query),
    ShowInList =
        case proplists:get_value("editshowinlist", Query) of
            [_ | _] -> true;
            _ -> false
        end,
    case echessd_lib:list_to_time_offset(StrTimezone) of
        {ok, Timezone} ->
            NewUserInfo =
                [{fullname, Fullname},
                 {timezone, Timezone},
                 {language, StrLanguage},
                 {show_in_list, ShowInList}],
            case echessd_user:setprops(
                   Username, NewUserInfo) of
                ok ->
                    echessd_session:read([{"sid", get(sid)}]),
                    process_get(
                      ?SECTION_HOME,
                      [{"goto", ?SECTION_HOME}], true);
                {error, Reason} ->
                    echessd_html:error(
                      gettext(txt_err_save_user) ++ ":~n~9999p",
                      [Reason])
            end;
        _ ->
            echessd_html:error(
              gettext(txt_err_save_user) ++ ":~n" ++
                  gettext(txt_err_bad_timezone), [])
    end;
process_post(?SECTION_NEWGAME, Query, true) ->
    {Opponent, _UserInfo} =
        echessd_session:get_val(opponent),
    Color =
        case proplists:get_value("color", Query) of
            "white" -> ?white;
            "black" -> ?black;
            _ ->
                echessd_lib:random_elem([?white, ?black])
        end,
    GameType0 = proplists:get_value("gametype", Query),
    GameType =
        case lists:member(GameType0, ?GAME_TYPES) of
            true -> GameType0;
            _ -> ?GAME_CLASSIC
        end,
    Private =
        case proplists:get_value("private", Query) of
            [_ | _] -> true;
            _ -> false
        end,
    Iam = get(username),
    case echessd_game:add(
           GameType, Iam, Color, Opponent,
           [{private, Private}]) of
        {ok, GameID} when Iam == Opponent ->
            process_get(
              ?SECTION_GAME,
              [{"goto", ?SECTION_GAME},
               {"game", integer_to_list(GameID)}], true);
        {ok, _GameID} ->
            process_get(
              ?SECTION_HOME, [{"goto", ?SECTION_HOME}], true);
        {error, Reason} ->
            echessd_html:error(
              gettext(txt_err_new_game) ++ ":~n~9999p", [Reason])
    end;
process_post(?SECTION_MOVE, Query, true) ->
    User = get(username),
    GameID = list_to_integer(get_query_item("game")),
    Ply =
        string:to_lower(
          echessd_lib:strip(
            proplists:get_value("move", Query),
            " \t\r\n")),
    case Ply of
        [_ | _] ->
            case echessd_game:ply(GameID, User, Ply) of
                ok -> nop;
                {error, not_your_turn} ->
                    %% silently ignore this
                    %% (maybe caused by old page update)
                    nop;
                Error -> put(error, Error)
            end;
        _ -> nop
    end,
    process_show(?SECTION_GAME);
process_post(?SECTION_DRAW, _Query, true) ->
    Username = get(username),
    GameID = list_to_integer(get_query_item("game")),
    case echessd_game:request_draw(GameID, Username) of
        ok -> nop;
        Error -> put(error, Error)
    end,
    process_show(?SECTION_GAME);
process_post(?SECTION_GIVEUP, _Query, true) ->
    Username = get(username),
    GameID = list_to_integer(get_query_item("game")),
    case echessd_game:give_up(GameID, Username) of
        ok -> nop;
        Error -> put(error, Error)
    end,
    process_show(?SECTION_GAME);
process_post(_, _, _) ->
    echessd_html:eaccess().

process_show() ->
    process_show(
      echessd_session:get_val(section)).
process_show(?SECTION_GAME) ->
    echessd_html:game(
      list_to_integer(get_query_item("game")));
process_show(?SECTION_HISTORY) ->
    echessd_html:history(
      list_to_integer(get_query_item("game")));
process_show(?SECTION_USERS) ->
    echessd_html:users();
process_show(?SECTION_USER) ->
    echessd_html:user(get_query_item("name"));
process_show(?SECTION_EDITUSER) ->
    echessd_html:edituser();
process_show(?SECTION_PASSWD_FORM) ->
    echessd_html:passwd();
process_show(?SECTION_NEWGAME) ->
    echessd_html:newgame();
process_show(?SECTION_DRAW_CONFIRM) ->
    echessd_html:draw_confirm(
      list_to_integer(get_query_item("game")));
process_show(?SECTION_GIVEUP_CONFIRM) ->
    echessd_html:giveup_confirm(
      list_to_integer(get_query_item("game")));
process_show(_Default) ->
    echessd_html:home().

process_goto(Query) ->
    String = proplists:get_value("goto", Query),
    Section =
        case lists:member(String, ?SECTIONS) of
            true -> String;
            _ -> ?SECTION_HOME
        end,
    echessd_session:set_val(section, Section).

get_query_item(Key) ->
    proplists:get_value(Key, get(query_proplist), "").

gettext(TextID) ->
    echessd_lib:gettext(TextID, get(language)).

