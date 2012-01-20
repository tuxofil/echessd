-module(echessd).

-export([start/0, stop/0]).

-include("echessd.hrl").

start() ->
    application:start(?MODULE, permanent).

stop() ->
    application:stop(?MODULE).

