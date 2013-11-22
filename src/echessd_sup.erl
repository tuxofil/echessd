%%% @doc
%%% Application main supervisor.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 20 Jan 2012
%%% @copyright 2012, Aleksey Morarash

-module(echessd_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Starts supervisor as part of a supervision tree.
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: any()}.
start_link() ->
    supervisor:start_link(?MODULE, no_args).

%% @doc Calls initialisation procedures and return child workers spec.
%% @hidden
-spec init(Args :: any()) ->
                  {ok,
                   {{RestartStrategy :: supervisor:strategy(),
                     MaxR :: non_neg_integer(),
                     MaxT :: non_neg_integer()},
                    [ChildSpec :: supervisor:child_spec()]}}.
init(_Args) ->
    true = register(?MODULE, self()),
    ok = echessd_db:wait(),
    {ok, {
       {one_for_one, 5, 1},
       [
        %% logger
        {echessd_log, {echessd_log, start_link, []},
         permanent, 100, worker, [echessd_log]},
        %% priv filesystem
        {echessd_priv, {echessd_priv, start_link, []},
         permanent, 100, worker, [echessd_priv]},
        %% lang strings keeper
        {echessd_lang, {echessd_lang, start_link, []},
         permanent, 100, worker, [echessd_lang]},
        %% styles keeper
        {echessd_styles, {echessd_styles, start_link, []},
         permanent, 100, worker, [echessd_styles]},
        %% configuration keeper
        {echessd_cfg, {echessd_cfg, start_link, []},
         permanent, 100, worker, [echessd_cfg]},
        %% mime types reference keeper
        {echessd_mime_types, {echessd_mime_types, start_link, []},
         permanent, 100, worker, [echessd_mime_types]},
        %% user session keeper
        {echessd_session, {echessd_session, start_link, []},
         permanent, 100, worker, [echessd_session]},
        %% HTTPD warden process
        {echessd_httpd_warden, {echessd_httpd_warden, start_link, []},
         permanent, 100, worker, [echessd_httpd_warden]}
       ]}}.
