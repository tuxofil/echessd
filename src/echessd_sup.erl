%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 20 Jan 2012
%%% @copyright 2012, Aleksey Morarash
%%% @doc Application main supervisor.

-module(echessd_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Starts supervisor as part of a supervision tree.
%% @spec start_link() -> {ok, pid()}
start_link() ->
    supervisor:start_link(?MODULE, no_args).

%% @doc Calls initialisation procedures and return child workers spec.
%% @spec init(StartArgs) -> {ok, WorkersSpec}
%%     StartArgs = term(),
%%     WorkersSpec = tuple()
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

        %% HTTPD warden process
        {echessd_web_warden,
         {echessd_web_warden, start_link, []},
         permanent, 100, worker, dynamic}
       ]}}.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

