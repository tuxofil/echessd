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
    if Method == ?HTTP_GET ->
            "/" ++ Path = Req:get(path),
            case Path of
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

