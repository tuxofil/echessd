-module(echessd_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-include("echessd.hrl").

start_link() ->
    supervisor:start_link(?MODULE, no_args).

init(_StartArgs) ->
    true = register(?MODULE, self()),
    ok = echessd_cfg:read(),
    ok = echessd_db:wait(),
    ok = echessd_session:init(),
    {ok, {
       {one_for_one, 5, 1},
       [
        %% logger
        {echessd_log,
         {echessd_log, start_link, []},
         permanent, 100, worker, [echessd_log]},

        %% HTTP request handler
        {echessd_srv,
         {echessd_srv, start_link, []},
         permanent, 100, worker, dynamic}
       ]}}.

