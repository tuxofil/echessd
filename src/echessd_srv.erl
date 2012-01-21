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
                    process_get(Req)
            end;
       Method == ?HTTP_POST ->
            process_post(Req);
       true ->
            Req:respond({501, [], []})
    end.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

process_get(Req) ->
    put(query_proplist, Query = Req:parse_qs()),
    echessd_log:debug("GET query=~9999p", [Query]),
    Action = proplists:get_value("action", Query),
    process_get(Req, Action, Query, get(logged_in)).
process_get(Req, ?SECTION_EXIT, _Query, true) ->
    echessd_session:del(get(sid)),
    Req:ok({?mime_text_html, echessd_html:login()});
process_get(Req, _, Query, true) ->
    process_goto(Query),
    process_show(Req);
process_get(Req, _, Query, _) ->
    case proplists:get_value("goto", Query) of
        ?SECTION_REG ->
            Req:ok({?mime_text_html, echessd_html:register()});
        _ ->
            Req:ok({?mime_text_html, echessd_html:login()})
    end.

process_post(Req) ->
    put(query_proplist, Query = Req:parse_post()),
    echessd_log:debug("POST query=~9999p", [Query]),
    Action = proplists:get_value("action", Query),
    process_post(Req, Action, Query, get(logged_in)).
process_post(Req, ?SECTION_LOGIN, Query, LoggedIn) ->
    Username = proplists:get_value("username", Query),
    Password = proplists:get_value("password", Query),
    case LoggedIn andalso get(username) == Username of
        true ->
            process_show(Req);
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
                    process_show(Req, [{"Set-Cookie", "sid=" ++ SID}]);
                _ ->
                    Req:ok({?mime_text_html, echessd_html:eaccess()})
            end
    end;
process_post(Req, ?SECTION_REG, Query, false) ->
    Username = proplists:get_value("regusername", Query),
    Password1 = proplists:get_value("regpassword1", Query),
    Password2 = proplists:get_value("regpassword2", Query),
    if Password1 /= Password2 ->
            Req:ok({?mime_text_html,
                    echessd_html:error("Password confirmation failed")});
       true ->
            case echessd_user:add(
                   Username,
                   [{password, Password1},
                    {created, now()}]) of
                ok ->
                    process_post(
                      Req, "login",
                      [{"username", Username},
                       {"password", Password1}], false);
                {error, Reason} ->
                    Req:ok({?mime_text_html,
                            echessd_html:error(
                              io_lib:format(
                                "Failed to create new user:<br><tt>~9999p</tt>",
                                [Reason]))})
            end
    end;
process_post(Req, "move", Query, true) ->
    process_goto(Query),
    process_show(Req);
process_post(Req, _, _, _) ->
    Req:ok({?mime_text_html, echessd_html:eaccess()}).

process_show(Req) ->
    process_show(Req, []).
process_show(Req, ExtraHeaders) ->
    Section = echessd_session:get_val(section),
    process_show(Req, ExtraHeaders, Section).
process_show(Req, ExtraHeaders, ?SECTION_GAME) ->
    Req:ok({?mime_text_html, ExtraHeaders, echessd_html:game(undefined)});
process_show(Req, ExtraHeaders, ?SECTION_USERS) ->
    Req:ok({?mime_text_html, ExtraHeaders, echessd_html:users()});
process_show(Req, ExtraHeaders, ?SECTION_TEST) ->
    Req:ok({?mime_text_html, ExtraHeaders, echessd_html:test_table()});
process_show(Req, ExtraHeaders, ?SECTION_USER) ->
    Req:ok({?mime_text_html, ExtraHeaders, echessd_html:user()});
process_show(Req, ExtraHeaders, _Default) ->
    Req:ok({?mime_text_html, ExtraHeaders, echessd_html:home()}).

process_goto(Query) ->
    String = proplists:get_value("goto", Query),
    Section =
        case lists:member(String, ?SECTIONS) of
            true -> String;
            _ -> ?SECTION_HOME
        end,
    echessd_session:set_val(section, Section).

