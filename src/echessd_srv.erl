-module(echessd_srv).

-export([start_link/0, loop/1]).

-include("echessd.hrl").

-define(HTTP_GET, 'GET').
-define(HTTP_POST, 'POST').

-define(mime_text_html, "text/html").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

start_link() ->
    BindAddr = echessd_cfg:get(?CFG_BIND_ADDR),
    BindPort = echessd_cfg:get(?CFG_BIND_PORT),
    {ok, _Pid} = Ok =
        mochiweb_http:start_link(
          [{name, ?MODULE},
           {loop, fun loop/1},
           {ip, BindAddr},
           {port, BindPort}
          ]),
    echessd_log:info(
      "HTTP server listens at ~s:~w",
      [echessd_lib:ip2str(BindAddr), BindPort]),
    Ok.

loop(Req) ->
    {ok, IP} =
        case inet_parse:address(Req:get(peer)) of
            {ok, _} = Ok -> Ok;
            {error, PeerReason} ->
                throw(PeerReason)
        end,
    Method = Req:get(method),
    Path = Req:get(path),
    Cookie = Req:parse_cookie(),
    echessd_log:debug(
      "~s> Method=~9999p; Path=~9999p; Cookie=~9999p",
      [echessd_lib:ip2str(IP), Method, Path, Cookie]),
    echessd_session:read(Cookie),
    put(doc_root, DocRoot = echessd_cfg:get(?CFG_DOC_ROOT)),
    if Method == ?HTTP_GET ->
            case lists:prefix("/res/", Path) of
                true ->
                    "/res/" ++ ResName = Path,
                    Req:serve_file(ResName, DocRoot);
                _ ->
                    safe_handle(Req, fun process_get/1)
            end;
       Method == ?HTTP_POST ->
            safe_handle(Req, fun process_post/1);
       true ->
            Req:respond({501, [], []})
    end.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

safe_handle(Req, Fun) ->
    erase(extra_headers),
    Content =
        try Fun(Req)
        catch
            _:{error, Reason} ->
                echessd_html:error(
                  io_lib:format(
                    "Response generation failed:<br><tt>~p</tt>",
                    [Reason]));
            Type:Reason ->
                StackTrace = erlang:get_stacktrace(),
                echessd_html:error(
                  io_lib:format(
                    "Response generation failed:<br><tt>~p</tt>",
                    [{Type, Reason, StackTrace}]))
        end,
    ExtraHeaders =
        case get(extra_headers) of
            [_ | _] = ExtraHeaders0 ->
                ExtraHeaders0;
            _ -> []
        end,
    try Req:ok({?mime_text_html, ExtraHeaders, Content})
    catch
        Type2:Reason2 ->
            StackTrace2 = erlang:get_stacktrace(),
            Req:ok({?mime_text_html, [],
                    echessd_html:error(
                      io_lib:format(
                        "Response sending failed:<br><tt>~p</tt>",
                        [{Type2, Reason2, StackTrace2}]))})
    end.

add_extra_headers(ExtraHeaders) ->
    put(extra_headers,
        case get(extra_headers) of
            [_ | _] = ExtraHeaders0 ->
                ExtraHeaders0;
            _ -> []
        end ++ ExtraHeaders).

process_get(Req) ->
    put(query_proplist, Query = Req:parse_qs()),
    echessd_log:debug("GET query=~9999p", [Query]),
    Action = proplists:get_value("action", Query),
    process_get(Action, Query, get(logged_in)).
process_get(?SECTION_EXIT, _Query, true) ->
    echessd_session:del(get(sid)),
    echessd_html:login();
process_get(_, Query, true) ->
    process_goto(Query),
    put(query_proplist, Query),
    process_show();
process_get(_, Query, _) ->
    case proplists:get_value("goto", Query) of
        ?SECTION_REG ->
            echessd_html:register();
        _ ->
            echessd_html:login()
    end.

process_post(Req) ->
    put(query_proplist, Query = Req:parse_post()),
    echessd_log:debug("POST query=~9999p", [Query]),
    Action = proplists:get_value("action", Query),
    process_post(Action, Query, get(logged_in)).
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
                    add_extra_headers([{"Set-Cookie", "sid=" ++ SID}]),
                    process_show();
                _ ->
                    echessd_html:eaccess()
            end
    end;
process_post(?SECTION_REG, Query, false) ->
    Username = proplists:get_value("regusername", Query),
    Password1 = proplists:get_value("regpassword1", Query),
    Password2 = proplists:get_value("regpassword2", Query),
    if Password1 /= Password2 ->
            echessd_html:error("Password confirmation failed");
       true ->
            case echessd_user:add(
                   Username,
                   [{password, Password1},
                    {created, now()}]) of
                ok ->
                    process_post(
                      ?SECTION_LOGIN,
                      [{"username", Username},
                       {"password", Password1}], false);
                {error, Reason} ->
                    echessd_html:error(
                      io_lib:format(
                        "Failed to create new user:<br><tt>~9999p</tt>",
                        [Reason]))
            end
    end;
process_post(?SECTION_NEWGAME, Query, true) ->
    {User, _UserInfo} = echessd_session:get_val(opponent),
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
    case echessd_game:add(GameType, get(username), Color, User, []) of
        {ok, ID} ->
            process_get(
              ?SECTION_GAME,
              [{"goto", ?SECTION_GAME},
               {"game", integer_to_list(ID)}], true);
        {error, Reason} ->
            echessd_html:error(
              io_lib:format(
                "Failed to create new game:<br><tt>~9999p</tt>",
                [Reason]))
    end;
process_post(?SECTION_MOVE, Query, true) ->
    User = get(username),
    Game = list_to_integer(get_query_item("game")),
    Move = string:to_lower(proplists:get_value("move", Query)),
    case echessd_game:move(Game, User, Move) of
        ok -> process_show(?SECTION_GAME);
        {error, Reason} ->
            echessd_html:error(
              io_lib:format(
                "Failed to make move:<br><tt>~9999p</tt>",
                [Reason]))
    end;
process_post(_, _, _) ->
    echessd_html:eaccess().

process_show() ->
    process_show(
      echessd_session:get_val(section)).
process_show(?SECTION_GAME) ->
    echessd_html:game(
      list_to_integer(get_query_item("game")));
process_show(?SECTION_USERS) ->
    echessd_html:users();
process_show(?SECTION_TEST) ->
    echessd_html:test_table(
      string:tokens(get_query_item("moves"), ","));
process_show(?SECTION_USER) ->
    echessd_html:user(get_query_item("name"));
process_show(?SECTION_NEWGAME) ->
    echessd_html:newgame();
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

