%%%-------------------------------------------------------------------
%%% File    : echessd_user.erl
%%% Author  : Aleksey Morarash <aleksey.morarash@gmail.com>
%%% Created : 21 Jan 2012
%%% License : FreeBSD
%%% Description : upper level user management tools
%%%
%%%-------------------------------------------------------------------

-module(echessd_user).

-export([list/0, add/2, del/1, auth/2, getprops/1, setprops/2]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% Type definitions
%% ----------------------------------------------------------------------

-export_type([echessd_user/0,
              echessd_user_info/0
             ]).

-type echessd_user() :: string().
%% Username format definition.

-type echessd_user_info() :: [echessd_user_property()].
%% User properties format definition.

-type echessd_user_property() ::
        {password, string()} |
        {created, erlang:timestamp()} |
        {login, echessd_user()} |
        {fullname, string()} |
        {games, [echessd_game:echessd_game_id()]}.
%% Some descr

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Return list of all users registered.
%% @spec list() -> [echessd_user()]
list() ->
    echessd_db:list_users().

%% @doc Adds new user to database.
%% @spec add(Username, UserProperties) -> ok | {error, Reason}
%%     Username = echessd_user(),
%%     UserProperties = echessd_user_info(),
%%     Reason = term()
add(Username, UserProperties0) ->
    Result =
        case is_valid_username(Username) of
            true ->
                case check_properties(UserProperties0) of
                    {error, _} = Error -> Error;
                    UserProperties ->
                        echessd_db:adduser(
                          Username, UserProperties)
                end;
            _ ->
                {error, {bad_username, Username}}
        end,
    case Result of
        ok ->
            echessd_log:info(
              "user ~9999p added (~9999p)",
              [Username, UserProperties0]);
        {error, Reason} ->
            echessd_log:err(
              "user ~9999p add failed: ~9999p",
              [Username, Reason])
    end,
    Result.

%% @doc Completely removes user from database.
%% @spec del(Username) -> ok | {error, Reason}
%%     Username = echessd_user(),
%%     Reason = term()
del(Username) ->
    case echessd_db:deluser(Username) of
        ok ->
            echessd_log:info("user ~9999p removed", [Username]),
            ok;
        {error, Reason} = Error ->
            echessd_log:err(
              "user ~9999p remove failed: ~9999p",
              [Username, Reason]),
            Error
    end.

%% @doc Make user authentication. Return user properties on success.
%% @spec auth(Username, Password) -> {ok, UserProperties} | {error, Reason}
%%     Username = echessd_user(),
%%     Password = string(),
%%     UserProperties = echessd_user_info(),
%%     Reason = term()
auth(Username, Password) when is_list(Password) ->
    auth(Username, crypto:sha(Password));
auth(Username, Password) when is_binary(Password) ->
    case getprops(Username) of
        {ok, UserProperties} = Ok ->
            case proplists:get_value(password, UserProperties) of
                Password ->
                    echessd_log:info(
                      "user ~9999p authenticated",
                      [Username]),
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
              [Username, Reason]),
            Error
    end.

%% @doc Fetch user properties from database.
%% @spec getprops(Username) -> {ok, UserProperties} | {error, Reason}
%%     Username = echessd_user(),
%%     UserProperties = echessd_user_info(),
%%     Reason = term()
getprops(Username) ->
    echessd_db:get_user_props(Username).

%% @doc Sets user properties.
%% @spec setprops(Username, UserProperties) -> ok | {error, Reason}
%%     Username = echessd_user(),
%%     UserProperties = echessd_user_info(),
%%     Reason = term()
setprops(Username, UserProperties0) ->
    Result =
        case check_properties(UserProperties0) of
            {error, _} = Error -> Error;
            UserProperties ->
                echessd_db:set_user_props(
                  Username, UserProperties)
        end,
    case Result of
        ok ->
            echessd_log:info("user ~9999p props updated", [Username]),
            ok;
        {error, Reason} = FinalError ->
            echessd_log:err(
              "user ~9999p props update failed: ~9999p",
              [Username, Reason]),
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
check_property({fullname, V}) ->
    {fullname, drop_html(lists:sublist(V, 70))};
check_property({K, _V}) ->
    throw({error, {unknown_property, K}}).

drop_html(String) ->
    lists:flatmap(
      fun($<) -> "&lt;";
         (C) -> [C]
      end, String).

