%%% @doc
%%% HTTPD related utilities

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 2 Feb 2012
%%% @copyright 2012, Aleksey Morarash

-module(echessd_httpd_lib).

-export(
   [add_extra_headers/1,
    get_extra_headers/0,
    reset_extra_headers/0
   ]).

%% ----------------------------------------------------------------------
%% Type definitions
%% ----------------------------------------------------------------------

-type headers() :: [{Key :: string(), Value :: string()}].

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc While generating a HTML page sometimes we need to send
%%      an additional HTTP headers before a page content.
%%      The function will add such header data to the process
%%      dictionary and after the page get generated, the
%%      headers will be sent to the client before the page.
-spec add_extra_headers(ExtraHeaders :: headers()) -> ok.
add_extra_headers(ExtraHeaders) ->
    _OldHeaders = put(extra_headers, get_extra_headers() ++ ExtraHeaders),
    ok.

%% @doc Fetch all extra HTTP headers, saved in the process
%%      dictionary by the add_extra_headers/1 fun.
%% @see add_extra_headers/1
-spec get_extra_headers() -> ExtraHeaders :: headers().
get_extra_headers() ->
    case get(extra_headers) of
        [_ | _] = Headers ->
            Headers;
        _ ->
            []
    end.

%% @doc Remove all headers from the process dictionary.
%% @see add_extra_headers/1
-spec reset_extra_headers() -> ok.
reset_extra_headers() ->
    _OldHeaders = erase(extra_headers),
    ok.
