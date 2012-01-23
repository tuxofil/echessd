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

start() ->
    application:start(?MODULE, permanent).

stop() ->
    application:stop(?MODULE).

