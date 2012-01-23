%%%-------------------------------------------------------------------
%%% File    : echessd.erl
%%% Author  : Aleksey Morarash <aleksey.morarash@gmail.com>
%%% Created : 20 Jan 2012
%%% License : FreeBSD
%%% Description : Application start/stop wrapper functions
%%%
%%%-------------------------------------------------------------------

-module(echessd).

-export([start/0, stop/0]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Load and start echessd.
%% @spec start() -> ok | {error, Reason}
%%     Reason = term()
start() ->
    application:start(?MODULE, permanent).

%% @doc Stop echessd.
%% @spec stop() -> ok | {error, Reason}
%%     Reason = term()
stop() ->
    application:stop(?MODULE).

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

