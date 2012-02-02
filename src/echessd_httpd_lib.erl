%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 2 Feb 2012
%%% @copyright 2012, Aleksey Morarash
%%% @doc http related utilities

-module(echessd_httpd_lib).

-export([reset_extra_headers/0,
         add_extra_headers/1,
         get_extra_headers/0
        ]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

reset_extra_headers() ->
    erase(extra_headers), ok.

add_extra_headers(ExtraHeaders) ->
    put(extra_headers,
        case get(extra_headers) of
            [_ | _] = ExtraHeaders0 ->
                ExtraHeaders0;
            _ -> []
        end ++ ExtraHeaders), ok.

get_extra_headers() ->
    case get(extra_headers) of
        [_ | _] = Headers -> Headers;
        _ -> []
    end.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

