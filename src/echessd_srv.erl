-module(echessd_srv).

-export([start_link/0, loop/1]).

-include("echessd.hrl").

-define(HTTP_GET, 'GET').

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
    FullPath = Req:get(path),
    Query = Req:parse_qs(),
    echessd_log:debug(
      "~s> Method=~9999p; Path=~9999p; Query=~9999p",
      [echessd_lib:ip2str(IP), Method, FullPath, Query]),
    if Method == ?HTTP_GET ->
            "/" ++ Path = FullPath,
            case Path of
                "table" ->
                    Game0 = echessd_game:new(?GAME_CLASSIC),
                    Moves = string:tokens(proplists:get_value("moves", Query), ","),
                    Game =
                        lists:foldl(
                          fun(Move, Acc) ->
                                  {T, _} = echessd_game:move(Acc, Move),
                                  T
                          end, Game0, Moves),
                    DocRoot = echessd_cfg:get(?CFG_DOC_ROOT),
                    Content =
                        echessd_lib:read_file(
                          filename:join(DocRoot, "header.html")) ++
                        echessd_html:table(Game) ++
                        echessd_lib:read_file(
                          filename:join(DocRoot, "footer.html")),
                    Req:ok({?mime_text_html, Content});
                _Other ->
                    %% fixme: following behaviour is only for debug purposes
                    DocRoot = echessd_cfg:get(?CFG_DOC_ROOT),
                    Req:serve_file(Path, DocRoot)
            end;
       true ->
            Req:respond({501, [], []})
    end.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

