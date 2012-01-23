%%%-------------------------------------------------------------------
%%% File    : echessd_app.erl
%%% Author  : Aleksey Morarash <aleksey.morarash@gmail.com>
%%% Created : 20 Jan 2012
%%% License : FreeBSD
%%% Description : 'application' behaviour module
%%%
%%%-------------------------------------------------------------------

-module(echessd_app).

-behaviour(application).

-export([start/2, start_phase/3, prep_stop/1, stop/1, config_change/3]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% API: 'application' behaviour callback functions
%% ----------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    echessd_sup:start_link().

start_phase(_Phase, _StartType, _PhaseArgs) ->
    ok.

prep_stop(_State) ->
    ok.

stop(_State) ->
    ok.

config_change(_Changed, _New, _Removed) ->
    ok.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

