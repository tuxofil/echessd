%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 1 Feb 2012
%%% @copyright 2012, Aleksey Morarash
%%% @doc inets-httpd interface

-module(echessd_httpd_inets).

-behaviour(echessd_httpd).

%% API exports
-export([start_link/3]).

%% httpd callbacks
-export([do/1, load/2, store/2, remove/1]).

-include("echessd.hrl").
-include_lib("kernel/include/file.hrl").

-ifndef(WITHOUT_INETS_HEADER).
-include_lib("inets/include/httpd.hrl").
-else.
-record(mod,{init_data,
             data=[],
             socket_type=ip_comm,
             socket,
             config_db,
             method,
             absolute_uri=[],
             request_uri,
             http_version,
             request_line,
             parsed_header=[],
             entity_body,
             connection}).
-endif.

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Start inets HTTP server.
%% @spec start_link(BindAddr, BindPort, DocRoot) -> {ok, Pid}
%%     BindAddr = ip_address(),
%%     BindPort = integer(),
%%     DocRoot = string(),
%%     Pid = pid()
start_link(BindAddr, BindPort, DocRoot) ->
    inets:start(
      httpd,
      [{port, BindPort},
       {bind_address,
        if BindAddr == {0,0,0,0} -> any;
           true -> BindAddr
        end},
       {server_name, "echessd"},
       {server_root, "."},
       {document_root, DocRoot},
       {ipfamily, inet},
       {modules, [echessd_httpd_inets]}
      ], stand_alone).

%% ----------------------------------------------------------------------
%% httpd callback functions
%% ----------------------------------------------------------------------

-define(HTTP_GET, "GET").
-define(HTTP_POST, "POST").

-define(mime_text_plain, "text/plain").
-define(mime_text_html, "text/html").

%% @hidden
do(ModData) ->
    case ModData#mod.request_uri of
        "/res/" ++ ResFile0 when ModData#mod.method == ?HTTP_GET ->
            %% serve static file
            {ResFile, _} = split4pathNquery(ResFile0),
            serve_file(ModData, ResFile);
        "/favicon.ico" ++ _ when ModData#mod.method == ?HTTP_GET ->
            serve_file(ModData, "favicon.ico");
        _ ->
            {_, GetQueryString} =
                split4pathNquery(ModData#mod.request_uri),
            Query =
                httpd:parse_query(
                  case ModData#mod.method of
                      ?HTTP_GET -> GetQueryString;
                      ?HTTP_POST -> ModData#mod.entity_body;
                      Other ->
                          throw({bad_method, Other})
                  end),
            echessd_session:read(parse_cookies(ModData)),
            process(ModData, Query)
    end.

%% @hidden
load(_Line, _AccIn) -> ok.

%% @hidden
store(OptVal, _Config) -> {ok, OptVal}.

%% @hidden
remove(_ConfigDB) -> ok.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

process(ModData, Query) ->
    echessd_httpd_lib:reset_extra_headers(),
    erase(error),
    Content =
        try
            if ModData#mod.method ==?HTTP_GET ->
                    echessd_request_processor:process_get(Query);
               true ->
                    echessd_request_processor:process_post(Query)
            end
        catch
            _:{error, Reason} ->
                echessd_html:error(
                  gettext(txt_resp_gen_error) ++ ":~n~p", [Reason]);
            Type:Reason ->
                echessd_html:error(
                  gettext(txt_resp_gen_error) ++ ":~n~p",
                  [{Type, Reason, erlang:get_stacktrace()}])
        end,
    try
        MimeType = ?mime_text_html,
        Binary = list_to_binary(Content),
        Headers =
            [{content_type, MimeType},
             {content_length, integer_to_list(size(Binary))}] ++
            echessd_httpd_lib:get_extra_headers(),
        httpd_response:send_header(ModData, 200, Headers),
        httpd_socket:deliver(
          ModData#mod.socket_type,
          ModData#mod.socket, Binary),
        {proceed,
         [{response, {already_sent, 200, size(Binary)}},
          {mime_type, MimeType} |
          ModData#mod.data]}
    catch
        Type2:Reason2 ->
            {break,
             [{response,
               {501,
                echessd_html:error(
                  gettext(txt_resp_send_error) ++ ":~n~p",
                  [{Type2, Reason2, erlang:get_stacktrace()}])}}]}
    end.

%% @doc Splits request URI to Path and Query strings (delimited by '?').
%% @spec split4pathNquery(RequestURI) -> {Path, Query}
%%     RequestURI = string(),
%%     Path = string(),
%%     Query = string()
split4pathNquery(RequestURI) ->
    split4pathNquery(RequestURI, []).
split4pathNquery([$? | Tail], Path) ->
    {lists:reverse(Path), Tail};
split4pathNquery([H | Tail], Path) ->
    split4pathNquery(Tail, [H | Path]);
split4pathNquery(_, Path) ->
    {lists:reverse(Path), []}.

parse_cookies(ModData) ->
    lists:flatmap(
      fun(Str0) ->
              Str = echessd_lib:strip(Str0, " "),
              case string:tokens(Str, "=") of
                  [Key, Val] -> [{Key, Val}];
                  _ -> []
              end
      end,
      string:tokens(
        proplists:get_value(
          "cookie", ModData#mod.parsed_header, []),
        ";")).

serve_file(ModData, Filename0) ->
    Filename =
        filename:join(echessd_cfg:get(?CFG_DOC_ROOT), Filename0),
    MimeType =
        %% todo: get mime type by filename suffix
        ?mime_text_plain,
    case file:read_file_info(Filename) of
        {ok, FileInfo} ->
            Headers =
                [{content_type, MimeType},
                 {content_length,
                  integer_to_list(FileInfo#file_info.size)},
                 {last_modified,
                  httpd_util:rfc1123_date(FileInfo#file_info.mtime)}
                ],
            case file:read_file(Filename) of
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
                       {404,
                        echessd_html:error(gettext(txt_http_404_err), [])}}]}
            end;
        {error, _Reason} ->
            {break,
             [{response,
               {404,
                echessd_html:error(gettext(txt_http_404_err), [])}}]}
    end.

gettext(TextID) ->
    echessd_lib:gettext(TextID, get(language)).

