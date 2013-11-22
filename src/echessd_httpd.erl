%%% @doc
%%% inets-httpd interface

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 1 Feb 2012
%%% @copyright 2012, Aleksey Morarash

-module(echessd_httpd).

%% API exports
-export([start_link/2]).

%% httpd callbacks
-export([do/1, load/2, store/2, remove/1]).

-include("echessd.hrl").
-include_lib("kernel/include/file.hrl").

%% ----------------------------------------------------------------------
%% Type definitions
%% ----------------------------------------------------------------------

-export_type(
   [cookies/0,
    cookie/0,
    method/0,
    result/0
   ]).

-type cookies() :: [cookie()].

-type cookie() :: {Key :: nonempty_string(), Value :: string()}.

-type method() :: nonempty_string(). %% ?HTTP_GET | ?HTTP_POST

-type result() ::
        {redirect, URL :: string()} |
        (HtmlPageContent :: iolist()).

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Start inets HTTP server.
-spec start_link(BindAddr :: inet:ip_address(),
                 BindPort :: inet:port_number()) ->
                        {ok, Pid :: pid()} | {error, Reason :: any()}.
start_link(BindAddr, BindPort) ->
    inets:start(
      httpd,
      [{bind_address,
        if BindAddr == {0,0,0,0} -> any;
           true -> BindAddr
        end},
       {port, BindPort},
       {server_name, "echessd"},
       {server_root, "."},
       {document_root, "."},
       {ipfamily, inet},
       {modules, [?MODULE]}
      ], stand_alone).

%% ----------------------------------------------------------------------
%% httpd callback functions
%% ----------------------------------------------------------------------

-define(mime_text_html, "text/html").

%% @hidden
-spec do(ModData :: #mod{}) ->
                {proceed, list()} |
                {break, list()}.
do(ModData) ->
    case ModData#mod.request_uri of
        "/res/" ++ ResFile0 when ModData#mod.method == ?HTTP_GET ->
            {ResFile, _} = echessd_lib:split4pathNquery(ResFile0),
            serve_internal_file(ModData, ResFile);
        "/favicon.ico" ++ _ when ModData#mod.method == ?HTTP_GET ->
            serve_internal_file(ModData, "favicon.ico");
        _ ->
            process(
              ModData,
              _Query = echessd_query_parser:parse(ModData),
              echessd_session:from_cookies(parse_cookies(ModData)))
    end.

%% @hidden
-spec load(Line :: string(),
           AccIn :: [{Option :: httpd:property(), Value :: any()}]) -> ok.
load(_Line, _AccIn) ->
    ok.

%% @hidden
-spec store(OptVal :: {Option :: httpd:property(), Value :: any()},
            Config :: [{Option :: httpd:property(), Value :: any()}]) ->
                   {ok, {Option :: httpd:property(), Value :: any()}}.
store(OptVal, _Config) ->
    {ok, OptVal}.

%% @hidden
-spec remove(ConfigDB :: ets:tab()) -> ok.
remove(_ConfigDB) ->
    ok.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc
-spec process(ModData :: #mod{},
              Query :: echessd_query_parser:http_query(),
              Session :: #session{}) ->
                     {proceed, NewData :: list()}.
process(ModData, Query, Session) ->
    ok = echessd_httpd_lib:reset_extra_headers(),
    erase(error),
    try
        send_reply(
          ModData,
          generate_html(ModData#mod.method, Query, Session))
    catch
        Type2:Reason2 ->
            {break,
             [{response,
               {501,
                geterr(txt_resp_send_error,
                       {Type2, Reason2, erlang:get_stacktrace()})}}]}
    end.

%% @doc Generate HTML
-spec generate_html(Method :: method(),
                    Query :: echessd_query_parser:http_query(),
                    Session :: #session{}) -> result().
generate_html(Method, Query, Session) ->
    ok = echessd_log:debug("~s query=~9999p", [Method, Query]),
    Action = proplists:get_value("action", Query),
    try
        if Method == ?HTTP_GET ->
                echessd_request_processor:get(Action, Query, Session);
           true ->
                echessd_request_processor:post(Action, Query, Session)
        end
    catch
        _:{error, Reason} ->
            geterr(txt_resp_gen_error, Reason);
        Type:Reason ->
            FinalReason = {Type, Reason, erlang:get_stacktrace()},
            geterr(txt_resp_gen_error, FinalReason)
    end.

%% @doc Send the generated page contents to the client.
-spec send_reply(ModData :: #mod{}, Result :: result()) ->
                        {proceed, NewData :: list()}.
send_reply(ModData, {redirect, URL}) ->
    ok = echessd_log:debug("redirecting to ~9999p...", [URL]),
    Body = echessd_html:redirection(URL),
    Binary = list_to_binary(Body),
    Headers =
        [{location, URL},
         {content_type, ?mime_text_html},
         {content_length, integer_to_list(size(Binary))}] ++
        echessd_httpd_lib:get_extra_headers(),
    httpd_response:send_header(ModData, 303, Headers),
    httpd_socket:deliver(
      ModData#mod.socket_type,
      ModData#mod.socket, Binary),
    {proceed,
     [{response, {already_sent, 303, size(Binary)}},
      {mime_type, ?mime_text_html} |
      ModData#mod.data]};
send_reply(ModData, IoList) ->
    Binary = list_to_binary(IoList),
    Headers =
        [{content_type, ?mime_text_html},
         {content_length, integer_to_list(size(Binary))}] ++
        echessd_httpd_lib:get_extra_headers(),
    httpd_response:send_header(ModData, 200, Headers),
    httpd_socket:deliver(
      ModData#mod.socket_type,
      ModData#mod.socket, Binary),
    {proceed,
     [{response, {already_sent, 200, size(Binary)}},
      {mime_type, ?mime_text_html} |
      ModData#mod.data]}.

%% ----------------------------------------------------------------------
%% low level tools

%% @doc
-spec parse_cookies(ModData :: #mod{}) -> Cookies :: cookies().
parse_cookies(ModData) ->
    lists:flatmap(
      fun(Str0) ->
              Str = echessd_lib:strip(Str0, " "),
              case string:tokens(Str, "=") of
                  [Key, Val] ->
                      [{Key, Val}];
                  _ ->
                      []
              end
      end,
      string:tokens(
        proplists:get_value(
          "cookie", ModData#mod.parsed_header, []),
        ";")).

%% @doc
-spec serve_internal_file(ModData :: #mod{}, Path :: file:filename()) ->
                                 {proceed, NewData :: list()} |
                                 {break, NewData :: list()}.
serve_internal_file(ModData, Path) ->
    WwwPath = filename:join("www", Path),
    MimeType =
        echessd_mime_types:lookup(
          echessd_lib:strip(filename:extension(Path), ".")),
    case echessd_priv:read_file_info(WwwPath) of
        {ok, FileInfo} ->
            Headers =
                [{content_type, MimeType},
                 {content_length,
                  integer_to_list(FileInfo#file_info.size)},
                 {last_modified,
                  httpd_util:rfc1123_date(FileInfo#file_info.mtime)}
                ],
            case echessd_priv:read_file(WwwPath) of
                {ok, Binary} ->
                    httpd_response:send_header(ModData, 200, Headers),
                    httpd_socket:deliver(
                      ModData#mod.socket_type,
                      ModData#mod.socket, Binary),
                    {proceed,
                     [{response,
                       {already_sent, 200, FileInfo#file_info.size}},
                      {mime_type, MimeType} |
                      ModData#mod.data]};
                {error, _Reason} ->
                    {break,
                     [{response,
                       {404, geterr(txt_http_404_err)}}]}
            end;
        {error, _Reason} ->
            {break,
             [{response,
               {404, geterr(txt_http_404_err)}}]}
    end.

%% @doc Get localized and formatted error message.
-spec geterr(TextID :: atom()) -> iolist().
geterr(TextID) ->
    echessd_html:error(gettext(TextID)).

%% @doc Get localized and formatted error message.
-spec geterr(TextID :: atom(), Reason :: any()) -> iolist().
geterr(TextID, Reason) ->
    echessd_html:error(gettext(TextID) ++ ":~n~p", [Reason]).

%% @doc
-spec gettext(TextID :: atom()) -> iolist().
gettext(TextID) ->
    echessd_lib:gettext(TextID, get(language)).
