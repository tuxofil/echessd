%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 2 Feb 2012
%%% @copyright 2012, Aleksey Morarash
%%% @doc mochiweb interface

-module(echessd_httpd_mochiweb).

-behaviour(echessd_httpdengine).

%% API exports
-export([start_link/3]).

%% mochiweb callbacks
-export([loop/1]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Start mochiweb HTTP server.
%% @spec start_link(BindAddr, BindPort, DocRoot) -> {ok, Pid}
%%     BindAddr = ip_address(),
%%     BindPort = integer(),
%%     DocRoot = string(),
%%     Pid = pid()
start_link(BindAddr, BindPort, _DocRoot) ->
    mochiweb_http:start_link(
      [{name, ?MODULE},
       {loop, fun loop/1},
       {ip, BindAddr},
       {port, BindPort}
      ]).

%% ----------------------------------------------------------------------
%% mochiweb callback functions
%% ----------------------------------------------------------------------

-define(HTTP_GET, 'GET').
-define(HTTP_POST, 'POST').

-define(mime_text_html, "text/html").

%% @doc Callback for HTTP request handling
%%      (Request is a parameterized module).
%% @hidden
%% @spec loop(Request) -> term()
%%     Request = mochiweb_request()
loop(Request) ->
    put(doc_root, DocRoot = echessd_cfg:get(?CFG_DOC_ROOT)),
    echessd_session:read(Request:parse_cookie()),
    case Request:get(method) of
        ?HTTP_GET ->
            Path = Request:get(path),
            case lists:prefix("/res/", Path) of
                true ->
                    "/res/" ++ ResName = Path,
                    Request:serve_file(ResName, DocRoot);
                _ -> process(Request)
            end;
        ?HTTP_POST -> process(Request);
        _ -> Request:respond({501, [], []})
    end.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

process(Req) ->
    echessd_httpd_lib:reset_extra_headers(),
    erase(error),
    Content =
        try
            case Req:get(method) of
                ?HTTP_GET ->
                    Query = Req:parse_qs(),
                    echessd_request_processor:process_get(Query);
                ?HTTP_POST ->
                    Query = Req:parse_post(),
                    echessd_request_processor:process_post(Query)
            end
        catch
            _:{error, Reason} ->
                echessd_html:error(
                  gettext(resp_gen_error) ++ ":~n~p", [Reason]);
            Type:Reason ->
                echessd_html:error(
                  gettext(resp_gen_error) ++ ":~n~p",
                  [{Type, Reason, erlang:get_stacktrace()}])
        end,
    Headers = echessd_httpd_lib:get_extra_headers(),
    try Req:ok({?mime_text_html, Headers, Content})
    catch
        Type2:Reason2 ->
            Req:ok(
              {?mime_text_html, [],
               echessd_html:error(
                 gettext(resp_send_error) ++ ":~n~p",
                 [{Type2, Reason2, erlang:get_stacktrace()}])})
    end.

gettext(TextID) ->
    echessd_lib:gettext(TextID, get(language)).

