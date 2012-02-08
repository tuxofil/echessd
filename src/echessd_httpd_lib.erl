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

%% @doc While generating HTML page sometimes we need to send
%%      additional HTTP headers before page content.
%%      This function will add such header data to process
%%      dictionary and after page get generated, this
%%      headers will be sent to client before the page.
%% @spec add_extra_headers(ExtraHeaders) -> ok
%%     ExtraHeaders = [{Key, Value}],
%%     Key = string(),
%%     Value = string()
add_extra_headers(ExtraHeaders) ->
    put(extra_headers,
        case get(extra_headers) of
            [_ | _] = ExtraHeaders0 ->
                ExtraHeaders0;
            _ -> []
        end ++ ExtraHeaders), ok.

%% @doc Fetches all extra HTTP headers, saved in process
%%      dictionary by calls to add_extra_headers/1 fun.
%% @see add_extra_headers/1
%% @spec get_extra_headers() -> ExtraHeaders
%%     ExtraHeaders = [{Key, Value}],
%%     Key = string(),
%%     Value = string()
get_extra_headers() ->
    case get(extra_headers) of
        [_ | _] = Headers -> Headers;
        _ -> []
    end.

%% @doc Removes from process dictionary all extra HTTP headers
%%      added earlier by add_extra_headers/1 fun calls.
%% @see add_extra_headers/1
%% @spec reset_extra_headers() -> ok
reset_extra_headers() ->
    erase(extra_headers), ok.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

