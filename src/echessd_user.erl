%%%-------------------------------------------------------------------
%%% File    : echessd_user.erl
%%% Author  : Aleksey Morarash <aleksey.morarash@gmail.com>
%%% Created : 20 Jan 2012
%%% License : FreeBSD
%%% Description : upper level user management tools
%%%
%%%-------------------------------------------------------------------

-module(echessd_user).

-export([list/0, add/2, del/1, auth/2, getprops/1, setprops/2]).

-include("echessd.hrl").

%% @doc Username format definition.
%% @type echessd_user() = string()

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

list() ->
    echessd_db:list_users().

add(Name, Properties0) ->
    Result =
        case is_valid_username(Name) of
            true ->
                case check_properties(Properties0) of
                    {error, _} = Error -> Error;
                    Properties ->
                        case echessd_db:adduser(Name, Properties) of
                            {ok, _} -> ok;
                            Error -> Error
                        end
                end;
            _ ->
                {error, {bad_username, Name}}
        end,
    case Result of
        ok ->
            echessd_log:info(
              "user ~9999p added (~9999p)",
              [Name, Properties0]);
        {error, Reason} ->
            echessd_log:err(
              "user ~9999p add failed: ~9999p",
              [Name, Reason])
    end,
    Result.

del(Name) ->
    case echessd_db:deluser(Name) of
        {ok, _} ->
            echessd_log:info("user ~9999p removed", [Name]),
            ok;
        {error, Reason} = Error ->
            echessd_log:err(
              "user ~9999p remove failed: ~9999p",
              [Name, Reason]),
            Error
    end.

auth(Name, Password) when is_list(Password) ->
    auth(Name, crypto:sha(Password));
auth(Name, Password) when is_binary(Password) ->
    case echessd_db:get_user_props(Name) of
        {ok, Props} = Ok ->
            case proplists:get_value(password, Props) of
                Password ->
                    echessd_log:info("user ~9999p authenticated", [Name]),
                    Ok;
                _ ->
                    echessd_log:err(
                      "user ~9999p authentication failed: "
                      "password incorrect"),
                    {error, password_incorrect}
            end;
        {error, Reason} = Error ->
            echessd_log:err(
              "user ~9999p authentication failed: ~9999p",
              [Name, Reason]),
            Error
    end.

getprops(Name) ->
    echessd_db:get_user_props(Name).

setprops(Name, Properties0) ->
    Result =
        case check_properties(Properties0) of
            {error, _} = Error -> Error;
            Properties ->
                echessd_db:set_user_props(Name, Properties)
        end,
    case Result of
        {ok, _} ->
            echessd_log:info("user ~9999p props updated", [Name]),
            ok;
        {error, Reason} = FinalError ->
            echessd_log:err(
              "user ~9999p props update failed: ~9999p",
              [Name, Reason]),
            FinalError
    end.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

is_valid_username([_ | _] = String) ->
    lists:all(
      fun(C) ->
              is_integer(C)
                  andalso
                    ((C >= $A andalso C =< $Z)
                     orelse (C >= $a andalso C =< $z)
                     orelse (C >= $0 andalso C =< $9)
                     orelse C == $.
                     orelse C == $_
                     orelse C == $-)
      end, String);
is_valid_username(_) -> false.

check_properties(Term) ->
    try lists:sort(check_properties_(Term))
    catch
        _:{error, _} = Error ->
            Error;
        Type:Reason ->
            {error, {Type, Reason, erlang:get_stacktrace()}}
    end.
check_properties_([{K, _V} = I | Tail]) when is_atom(K) ->
    [check_property(I) | check_properties_(Tail)];
check_properties_([H | _]) ->
    throw({error, {bad_property, H}});
check_properties_([]) -> [];
check_properties_(Properties) ->
    throw({error, {not_a_list, Properties}}).

check_property({password = K, V} = I) ->
    if is_binary(V) -> I;
       is_list(V) -> {K, crypto:sha(V)};
       true -> throw({error, {badval, K, V}})
    end;
check_property({created, V} = I) ->
    if ?is_now(V) -> I;
       true ->
            throw({error, {bad_creation_time, V}})
    end;
check_property({games, V} = I) ->
    if is_list(V) ->
            lists:foreach(
              fun(N) when ?is_now(N) -> ok;
                 (N) ->
                      throw({error, {bad_game_id, N}})
              end, V),
            I;
       true ->
            throw({error, {bad_games_list, V}})
    end;
check_property({K, _V}) ->
    throw({error, {unknown_property, K}}).

