%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 2 Feb 2012
%%% @copyright 2012, Aleksey Morarash
%%% @doc HTTPD engine behaviour

-module(echessd_httpd).

-export([behaviour_info/1]).

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @hidden
behaviour_info(callbacks) ->
    [{start_link, 3}];
behaviour_info(_) ->
    undefined.

