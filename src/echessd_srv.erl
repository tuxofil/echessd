-module(echessd_srv).

-export([start_link/0, loop/1]).

-include("echessd.hrl").

-define(HTTP_GET, 'GET').

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
    Query = Req:parse_qs(),
    echessd_log:debug("Query=~9999p", [Query]),
    if Method == ?HTTP_GET ->
            "/" ++ Path = Req:get(path),
            case Path of
                "table" ->
                    Game0 = echessd_game:new(),
                    Moves = string:tokens(proplists:get_value("moves", Query), ","),
                    Game =
                        lists:foldl(
                          fun(Move, Acc) ->
                                  {T, _} = echessd_game:move(Acc, Move),
                                  T
                          end, Game0, Moves),
                    Content =
                        echessd_lib:read_file("www/header.html") ++
                        echessd_html:table(Game) ++
                        echessd_lib:read_file("www/footer.html"),
                    Req:ok({"text/html", Content});
                _Other ->
                    %% fixme: following behaviour is only for debug purposes
                    Req:serve_file(Path, "www")
            end;
       true ->
            Req:respond({501, [], []})
    end.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc Replaces all matched by Regexp text in String by Replacement
%% @spec str_replace(String, Regexp, Replacement) -> NewString
%%     String = Regexp = Replacement = NewString = string()
str_replace(String, Regexp, Replacement)
  when is_list(String), is_list(Regexp), is_list(Replacement) ->
    try re:replace( String, Regexp, Replacement, [global, {return, list}]) of
        Result -> Result
    catch
        _:_ -> String
    end.

