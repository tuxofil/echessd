%%% @doc
%%% User management tools.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 21 Jan 2012
%%% @copyright 2012, Aleksey Morarash

-module(echessd_user).

%% API exports
-export(
   [list/0, add/2, del/1, auth/2, getprops/1, setprops/2,
    passwd/2, get_value/2, default/1
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
        {?ui_login, name()} |
        {?ui_password, ((ClearPassword :: nonempty_string()) |
                        (PasswordHash :: encrypted_password()))} |
        {?ui_created, erlang:timestamp()} |
        {?ui_fullname, string()} |
        {?ui_timezone, echessd_lib:administrative_offset()} |
        {?ui_language, LangID :: atom()} |
        {?ui_show_in_list, boolean()} |
        {?ui_show_history, boolean()} |
        {?ui_show_comment, boolean()} |
        {?ui_notify, boolean()} |
        {?ui_auto_refresh, boolean()} |
        {?ui_auto_refresh_period, pos_integer()} |
        {?ui_style, StyleID :: atom()} |
        {?ui_jid, string()} |
        {?ui_games, [echessd_game:id()]}.

-type encrypted_password() ::
        legacy_sha1_encrypted_password() |
        {Algo :: password_encryption_algo(),
         Salt :: binary(),
         EncryptedPassword :: binary()}.

-type password_encryption_algo() :: sha.

-type legacy_sha1_encrypted_password() :: binary().

-type info_item_key() ::
        ?ui_password | ?ui_created | ?ui_login | ?ui_fullname | ?ui_timezone |
        ?ui_language | ?ui_show_in_list | ?ui_show_history | ?ui_show_comment |
        ?ui_notify | ?ui_auto_refresh | ?ui_auto_refresh_period | ?ui_style |
        ?ui_jid | ?ui_games.

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
                {error, {?e_bad_username, Name}}
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
-spec auth(Name :: name(), ClearPassword :: nonempty_string()) ->
                  {ok, Info :: info()} | {error, Reason :: any()}.
auth(Name, ClearPassword) ->
    case getprops(Name) of
        {ok, Info} = Ok ->
            case compare_password(
                   get_value(?ui_password, Info), ClearPassword) of
                true ->
                    echessd_log:info(
                      "user ~9999p authenticated",
                      [Name]),
                    Ok;
                false ->
                    echessd_log:err(
                      "user ~9999p authentication failed: ~9999p",
                      [Name, ?e_password_incorrect]),
                    {error, ?e_password_incorrect}
            end;
        {error, Reason} = Error ->
            echessd_log:err(
              "user ~9999p authentication failed: ~9999p",
              [Name, Reason]),
            Error
    end.

%% @doc
-spec compare_password(Encrypted :: encrypted_password(),
                       ClearPassword :: nonempty_string()) ->
                              boolean().
compare_password(Encrypted, ClearPassword)
 when is_binary(Encrypted) ->
    %% legacy encrypted password
    Encrypted == crypto:sha(ClearPassword);
compare_password({sha, Salt, Encrypted}, ClearPassword) ->
    Encrypted == crypto:sha([Salt, ClearPassword]).

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

%% @doc Set a new password for the user.
-spec passwd(Name :: name(), Password :: nonempty_string()) ->
                    ok | {error, Reason :: any()}.
passwd(Name, Password) ->
    setprops(Name, [{?ui_password, Password}]).

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
default(?ui_show_in_list) ->
    true;
default(?ui_show_history) ->
    true;
default(?ui_show_comment) ->
    true;
default(?ui_notify) ->
    true;
default(?ui_auto_refresh) ->
    false;
default(?ui_auto_refresh_period) ->
    60;
default(?ui_language) ->
    echessd_cfg:get(?CFG_DEF_LANG);
default(?ui_style) ->
    echessd_cfg:get(?CFG_DEF_STYLE);
default(?ui_timezone) ->
    echessd_lib:local_offset();
default(?ui_jid) ->
    "";
default(?ui_fullname) ->
    "";
default(?ui_games) ->
    [];
default(_) ->
    undefined.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc
-spec hide_sensitive(Info :: info()) -> NewInfo :: info().
hide_sensitive(Info) ->
    lists:map(
      fun({?ui_password = Key, _}) ->
              {Key, '*********'};
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
            throw({error, {?e_bad_info_item, Term}})
    end.

%% @doc
-spec check_property_(InfoItem :: info_item()) -> Value :: any().
check_property_({?ui_password, V}) when is_binary(V) ->
    V;
check_property_({?ui_password, {Algo, Salt, Encrypted} = V})
  when is_atom(Algo), is_binary(Salt), is_binary(Encrypted) ->
    V;
check_property_({?ui_password, V}) when is_list(V) ->
    true = is_string(V),
    Salt = crypto:rand_bytes(4),
    {sha, Salt, crypto:sha([Salt, V])};
check_property_({?ui_created, V}) when ?is_now(V) ->
    V;
check_property_({?ui_games, V}) ->
    true = lists:all(fun(I) when is_integer(I), I > 0 -> true end, V),
    V;
check_property_({?ui_fullname, V}) ->
    true = is_string(V),
    lists:sublist(V, 70);
check_property_({?ui_timezone, V}) ->
    true = lists:member(V, echessd_lib:administrative_offsets()),
    V;
check_property_({?ui_language, V}) ->
    true = lists:member(V, [N || {N, _} <- echessd_lang:list()]),
    V;
check_property_({?ui_show_in_list, V}) when is_boolean(V) ->
    V;
check_property_({?ui_show_history, V}) when is_boolean(V) ->
    V;
check_property_({?ui_show_comment, V}) when is_boolean(V) ->
    V;
check_property_({?ui_notify, V}) when is_boolean(V) ->
    V;
check_property_({?ui_auto_refresh, V}) when is_boolean(V) ->
    V;
check_property_({?ui_auto_refresh_period, V}) when is_integer(V), V >= 0 ->
    V;
check_property_({?ui_style, V}) ->
    true = lists:member(V, [N || {N, _} <- echessd_styles:list()]),
    V;
check_property_({?ui_jid, V}) ->
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
