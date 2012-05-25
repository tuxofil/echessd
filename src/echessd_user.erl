%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 21 Jan 2012
%%% @copyright 2012, Aleksey Morarash
%%% @doc upper level user management tools

-module(echessd_user).

-export([list/0, add/2, del/1, auth/2, getprops/1, setprops/2,
         lang_info/1]).

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
        {timezone, echessd_lib:administrative_offset()} |
        {language, atom()} |
        {show_in_list, boolean()} |
        {show_history, boolean()} |
        {show_comment, boolean()} |
        {notify, boolean()} |
        {auto_refresh, boolean()} |
        {auto_refresh_period, integer()} |
        {style, atom()} |
        {jid, string()} |
        {games, [echessd_game:echessd_game_id()]}.

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Return list of all users registered.
%% @spec list() -> [echessd_user()]
list() ->
    echessd_db:list_users().

%% @doc Adds new user to database.
%% @spec add(Username, UserInfo) -> ok | {error, Reason}
%%     Username = echessd_user(),
%%     UserInfo = echessd_user_info(),
%%     Reason = term()
add(Username, UserInfo0) ->
    Result =
        case is_valid_username(Username) of
            true ->
                case check_properties(UserInfo0) of
                    {error, _} = Error -> Error;
                    UserInfo ->
                        echessd_db:adduser(
                          Username, UserInfo)
                end;
            _ ->
                {error, {bad_username, Username}}
        end,
    case Result of
        ok ->
            echessd_log:info(
              "user ~9999p added (~9999p)",
              [Username, hide_sensitive(UserInfo0)]);
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
%% @spec auth(Username, Password) -> {ok, UserInfo} | {error, Reason}
%%     Username = echessd_user(),
%%     Password = string(),
%%     UserInfo = echessd_user_info(),
%%     Reason = term()
auth(Username, Password) when is_list(Password) ->
    auth(Username, crypto:sha(Password));
auth(Username, Password) when is_binary(Password) ->
    case getprops(Username) of
        {ok, UserInfo} = Ok ->
            case proplists:get_value(password, UserInfo) of
                Password ->
                    echessd_log:info(
                      "user ~9999p authenticated",
                      [Username]),
                    Ok;
                _ ->
                    Reason = password_incorrect,
                    echessd_log:err(
                      "user ~9999p authentication failed: ~9999p",
                      [Username, Reason]),
                    {error, Reason}
            end;
        {error, Reason} = Error ->
            echessd_log:err(
              "user ~9999p authentication failed: ~9999p",
              [Username, Reason]),
            Error
    end.

%% @doc Fetch user properties from database.
%% @spec getprops(Username) -> {ok, UserInfo} | {error, Reason}
%%     Username = echessd_user(),
%%     UserInfo = echessd_user_info(),
%%     Reason = term()
getprops(Username) ->
    echessd_db:get_user_props(Username).

%% @doc Sets user properties.
%% @spec setprops(Username, UserInfo) -> ok | {error, Reason}
%%     Username = echessd_user(),
%%     UserInfo = echessd_user_info(),
%%     Reason = term()
setprops(Username, UserInfo0) ->
    Result =
        case check_properties(UserInfo0) of
            {error, _} = Error -> Error;
            UserInfo ->
                echessd_db:set_user_props(
                  Username, UserInfo)
        end,
    case Result of
        ok ->
            echessd_log:info(
              "user ~9999p props updated: ~9999p",
              [Username, hide_sensitive(UserInfo0)]),
            ok;
        {error, Reason} = FinalError ->
            echessd_log:err(
              "user ~9999p props update failed: ~9999p",
              [Username, Reason]),
            FinalError
    end.

%% @doc Fetch language information from users info.
%% @spec lang_info(UserInfo) -> {LangAbbr, LangName}
%%     UserInfo = echessd_user_info(),
%%     LangAbbr = atom(),
%%     LangName = string()
lang_info(UserInfo) ->
    LangAbbr = proplists:get_value(language, UserInfo),
    Languages = echessd_lib:languages(),
    case proplists:get_value(LangAbbr, Languages) of
        [_ | _] = LangName ->
            {LangAbbr, LangName};
        _ ->
            DefLangAbbr = echessd_cfg:get(?CFG_DEF_LANG),
            case proplists:get_value(DefLangAbbr, Languages) of
                [_ | _] = LangName ->
                    {DefLangAbbr, LangName};
                _ ->
                    throw({error, no_default_language})
            end
    end.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

hide_sensitive(UserInfo) ->
    lists:map(
      fun({password, _}) ->
              {password, '*********'};
         (Other) -> Other
      end, UserInfo).

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
    {fullname, lists:sublist(V, 70)};
check_property({timezone, V} = I) ->
    case lists:member(V, echessd_lib:administrative_offsets()) of
        true -> I;
        _ -> throw({error, {bad_timezone, V}})
    end;
check_property({language = Key, A} = I) ->
    if is_atom(A) -> I;
       is_list(A) ->
            case echessd_lib:parse_language(A) of
                {LangAbbr, _LangName} ->
                    {Key, LangAbbr};
                _ ->
                    throw({error, {unsupported_language, A}})
            end;
       true ->
            throw({error, {bad_language, A}})
    end;
check_property({show_in_list, B} = I) ->
    if is_boolean(B) -> I;
       true ->
            throw({error, {bad_show_in_list, B}})
    end;
check_property({show_history, B} = I) ->
    if is_boolean(B) -> I;
       true ->
            throw({error, {bad_show_history, B}})
    end;
check_property({show_comment, B} = I) ->
    if is_boolean(B) -> I;
       true ->
            throw({error, {bad_show_comment, B}})
    end;
check_property({notify, B} = I) ->
    if is_boolean(B) -> I;
       true ->
            throw({error, {bad_notify, B}})
    end;
check_property({auto_refresh, B} = I) ->
    if is_boolean(B) -> I;
       true ->
            throw({error, {bad_auto_refresh, B}})
    end;
check_property({auto_refresh_period, Int} = I) ->
    if is_integer(Int) andalso Int > 0 -> I;
       true ->
            throw({error, {bad_auto_refresh_period, Int}})
    end;
check_property({style, S} = I) ->
    RegisteredStyles = [N || {N, _T, _F} <- echessd_lib:styles()],
    case lists:member(S, RegisteredStyles) of
        true -> I;
        _ ->
            {ID, _TextID, _Filename} = echessd_lib:default_style(),
            {style, ID}
    end;
check_property({jid, _S} = I) -> I;
check_property({K, _V}) ->
    throw({error, {unknown_property, K}}).

