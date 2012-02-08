%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 20 Jan 2012
%%% @copyright 2012, Aleksey Morarash
%%% @doc 'application' behaviour module.

-module(echessd_app).

-behaviour(application).

-export([start/2, start_phase/3, prep_stop/1, stop/1, config_change/3]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% API: 'application' behaviour callback functions
%% ----------------------------------------------------------------------

%% @hidden
start(_StartType, _StartArgs) ->
    echessd_sup:start_link().

%% @hidden
start_phase(_Phase, _StartType, _PhaseArgs) ->
    ok.

%% @hidden
prep_stop(_State) ->
    ok.

%% @hidden
stop(_State) ->
    ok.

%% @hidden
config_change(_Changed, _New, _Removed) ->
    ok.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

