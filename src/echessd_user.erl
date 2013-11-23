%%% @doc
%%% User management tools.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 21 Jan 2012
%%% @copyright 2012, Aleksey Morarash

-module(echessd_user).

%% API exports
-export(
   [list/0, add/2, del/1, auth/2, getprops/1, setprops/2,
    get_value/2, default/1,
    lang_info/1
   ]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% Type definitions
%% ----------------------------------------------------------------------

-export_type([name/0, info/0, info_item/0, info_item_key/0]).

-type name() :: nonempty_string().
%% Username format definition.

-type info() :: [info_item()].
%% User properties format definition.

-type info_item() ::
        {login, name()} |
        {password, ((ClearPassword :: nonempty_string()) |
                    (PasswordHash :: binary()))} |
        {created, erlang:timestamp()} |
        {fullname, string()} |
        {timezone, echessd_lib:administrative_offset()} |
        {language, LangID :: atom()} |
        {show_in_list, boolean()} |
        {show_history, boolean()} |
        {show_comment, boolean()} |
        {notify, boolean()} |
        {auto_refresh, boolean()} |
        {auto_refresh_period, pos_integer()} |
        {style, StyleID :: atom()} |
        {jid, string()} |
        {games, [echessd_game:id()]}.

-type info_item_key() ::
        password | created | login | fullname | timezone |
        language | show_in_list | show_history | show_comment |
        notify | auto_refresh | auto_refresh_period | style |
        jid | games.

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Return a list of all registered users.
-spec list() -> [name()].
list() ->
    {ok, Names} = echessd_db:list_users(),
    Names.

%% @doc Add a new user to the database.
-spec add(Name :: name(), Info :: info()) ->
                 ok | {error, Reason :: any()}.
add(Name, Info0) ->
    Result =
        case is_valid_username(Name) of
            true ->
                case check_properties(Info0) of
                    {ok, Info} ->
                        echessd_db:adduser(Name, Info);
                    {error, _} = Error ->
                        Error
                end;
            _ ->
                {error, {bad_username, Name}}
        end,
    case Result of
        ok ->
            echessd_log:info(
              "user ~9999p added (~9999p)",
              [Name, hide_sensitive(Info0)]);
        {error, Reason} ->
            echessd_log:err(
              "user ~9999p add failed: ~9999p",
              [Name, Reason])
    end,
    Result.

%% @doc Remove the user from the database.
-spec del(Name :: name()) -> ok | {error, Reason :: any()}.
del(Name) ->
    case echessd_db:deluser(Name) of
        ok ->
            echessd_log:info("user ~9999p removed", [Name]),
            ok;
        {error, Reason} = Error ->
            echessd_log:err(
              "user ~9999p remove failed: ~9999p",
              [Name, Reason]),
            Error
    end.

%% @doc Authenticate the user. Return user properties on success.
-spec auth(Name :: name(),
           Password :: (PlainPassword :: nonempty_string()) |
                       (PasswordHash :: binary())) ->
                  {ok, Info :: info()} | {error, Reason :: any()}.
auth(Name, Password) when is_list(Password) ->
    auth(Name, crypto:sha(Password));
auth(Name, Password) when is_binary(Password) ->
    case getprops(Name) of
        {ok, Info} = Ok ->
            case get_value(password, Info) of
                Password ->
                    echessd_log:info(
                      "user ~9999p authenticated",
                      [Name]),
                    Ok;
                _ ->
                    Reason = password_incorrect,
                    echessd_log:err(
                      "user ~9999p authentication failed: ~9999p",
                      [Name, Reason]),
                    {error, Reason}
            end;
        {error, Reason} = Error ->
            echessd_log:err(
              "user ~9999p authentication failed: ~9999p",
              [Name, Reason]),
            Error
    end.

%% @doc Fetch the user properties from the database.
-spec getprops(Name :: name()) ->
                      {ok, Info :: info()} |
                      {error, Reason :: any()}.
getprops(Name) ->
    echessd_db:get_user_props(Name).

%% @doc Set the user properties.
-spec setprops(Name :: name(), Info :: info()) ->
                      ok | {error, Reason :: any()}.
setprops(Name, Info0) ->
    Result =
        case check_properties(Info0) of
            {ok, Info} ->
                echessd_db:set_user_props(Name, Info);
            {error, _} = Error ->
                Error
        end,
    case Result of
        ok ->
            echessd_log:info(
              "user ~9999p props updated: ~9999p",
              [Name, hide_sensitive(Info0)]),
            ok;
        {error, Reason} = FinalError ->
            echessd_log:err(
              "user ~9999p props update failed: ~9999p",
              [Name, Reason]),
            FinalError
    end.

%% @doc Fetch a value from the user properties.
%% Same as proplists:get_value/2, but substitute
%% reasonable default value when value for the Key is not set.
-spec get_value(Key :: info_item_key(), Info :: info()) ->
                       Value :: any().
get_value(Key, Info) when is_list(Info) ->
    proplists:get_value(Key, Info, default(Key));
get_value(Key, _) ->
    default(Key).

%% @doc Return default value for an user property.
-spec default(Key :: info_item_key()) -> (Value :: any()) | undefined.
default(show_in_list) ->
    true;
default(show_history) ->
    true;
default(show_comment) ->
    true;
default(notify) ->
    true;
default(auto_refresh) ->
    false;
default(auto_refresh_period) ->
    60;
default(language) ->
    echessd_cfg:get(?CFG_DEF_LANG);
default(style) ->
    echessd_cfg:get(?CFG_DEF_STYLE);
default(timezone) ->
    echessd_lib:local_offset();
default(jid) ->
    "";
default(fullname) ->
    "";
default(games) ->
    [];
default(_) ->
    undefined.

%% @doc Fetch the language information from the user's info.
-spec lang_info(Info :: info()) ->
                       {LangID :: atom(),
                        LangName :: nonempty_string()}.
lang_info(Info) ->
    LangID = get_value(language, Info),
    Languages = echessd_lang:list(),
    case proplists:get_value(LangID, Languages) of
        [_ | _] = LangName ->
            {LangID, LangName};
        _ ->
            DefLangID = echessd_cfg:get(?CFG_DEF_LANG),
            case proplists:get_value(DefLangID, Languages) of
                [_ | _] = LangName ->
                    {DefLangID, LangName};
                _ ->
                    throw({error, no_default_language})
            end
    end.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc
-spec hide_sensitive(Info :: info()) -> NewInfo :: info().
hide_sensitive(Info) ->
    lists:map(
      fun({password, _}) ->
              {password, '*********'};
         (Other) ->
              Other
      end, Info).

%% @doc
-spec is_valid_username(Term :: any()) -> boolean().
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
is_valid_username(_) ->
    false.

%% @doc
-spec check_properties(Term :: any()) ->
                              {ok, Info :: info()} |
                              {error, Reason :: any()}.
check_properties(Term) ->
    try
        {ok, lists:map(fun check_property/1, Term)}
    catch
        _:{error, _} = Error ->
            Error;
        Type:Reason ->
            {error, {Type, Reason, erlang:get_stacktrace()}}
    end.

%% @doc
-spec check_property(Term :: any()) -> InfoItem :: info_item().
check_property(Term) ->
    try
        NewValue = check_property_({Key, _OldValue} = Term),
        {Key, NewValue}
    catch
        _:_ ->
            throw({error, {bad_info_item, Term}})
    end.

%% @doc
-spec check_property_(InfoItem :: info_item()) -> Value :: any().
check_property_({password, V}) when is_binary(V) ->
    V;
check_property_({password, V}) when is_list(V) ->
    true = is_string(V),
    crypto:sha(V);
check_property_({created, V}) when ?is_now(V) ->
    V;
check_property_({games, V}) ->
    true = lists:all(fun(I) when is_integer(I), I > 0 -> true end, V),
    V;
check_property_({fullname, [_ | _] = V}) ->
    true = is_string(V),
    lists:sublist(V, 70);
check_property_({timezone, V}) ->
    true = lists:member(V, echessd_lib:administrative_offsets()),
    V;
check_property_({language, V}) ->
    true = lists:member(V, [N || {N, _} <- echessd_lang:list()]),
    V;
check_property_({show_in_list, V}) when is_boolean(V) ->
    V;
check_property_({show_history, V}) when is_boolean(V) ->
    V;
check_property_({show_comment, V}) when is_boolean(V) ->
    V;
check_property_({show_notify, V}) when is_boolean(V) ->
    V;
check_property_({auto_refresh, V}) when is_boolean(V) ->
    V;
check_property_({auto_refresh_period, V}) when is_integer(V), V >= 0 ->
    V;
check_property_({style, V}) ->
    true = lists:member(V, [N || {N, _} <- echessd_styles:list()]),
    V;
check_property_({jid, V}) ->
    true = is_string(V),
    V.

%% @doc
-spec is_string(List :: any()) -> boolean().
is_string(List) when is_list(List) ->
    lists:all(
      fun(C) when is_integer(C), C >= $\s, C =< 16#ffff ->
              true;
         (_) ->
              false
      end, List);
is_string(_) ->
    false.
