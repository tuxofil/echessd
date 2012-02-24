%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 20 Jan 2012
%%% @copyright 2012, Aleksey Morarash
%%% @doc Configuration file parsing and configuration
%%%   items accessing tools.

-module(echessd_cfg).

-export([read/0, get/1, default/1]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Read configuration file (obtained via VM command line argument)
%%      and store configuration items in ETS table.
%% @spec read() -> ok
read() ->
    catch ets:new(?echessd_cfg, [named_table, public, set]),
    echessd_log:info("Reading language file..."),
    LangInfo =
        case read_lang_file() of
            {ok, LangInfo0} -> LangInfo0;
            {error, Reason0} ->
                echessd_log:err(
                  "Lang file parse error: ~99999p",
                  [Reason0]),
                throw({lang_read_error, Reason0})
        end,
    ets:insert(?echessd_cfg, {?CFG_LANG_INFO, LangInfo}),
    echessd_log:info("Reading styles definitions..."),
    StylesInfo =
        case read_styles_file() of
            {ok, StylesInfo0} -> StylesInfo0;
            {error, Reason1} ->
                echessd_log:err(
                  "Styles file parse error: ~99999p",
                  [Reason1]),
                throw({styles_read_error, Reason1})
        end,
    ets:insert(?echessd_cfg, {?CFG_STYLES_INFO, StylesInfo}),
    lists:foreach(
      fun(CfgItem) ->
              catch ets:delete(?echessd_cfg, CfgItem)
      end, ?CFGS),
    echessd_log:info("Reading configurations..."),
    Args = init:get_arguments(),
    CfgFile =
        case proplists:get_value(echessd_config, Args) of
            [CfgFile0 | _] ->
                CfgFile0;
            _ ->
                echessd_log:err(
                  "You must specify configuration filename "
                  "with '-echessd_config' option!"),
                throw(no_cfg_filename)
        end,
    case file:read_file(CfgFile) of
        {ok, Binary} ->
            String = binary_to_list(Binary),
            case parse_config(String) of
                {ok, Config} ->
                    ets:insert(?echessd_cfg, Config),
                    ok;
                {error, Reason} ->
                    echessd_log:err(
                      "Configuration file ~99999p parse error: ~99999p",
                      [CfgFile, Reason]),
                    throw({cfg_read_error, Reason})
            end;
        {error, Reason} ->
            echessd_log:err(
              "Configuration file ~99999p read error: ~99999p",
              [CfgFile, Reason]),
            throw({cfg_read_error, Reason})
    end.

%% @doc Fetch configuration parameter from config (ETS).
%% @spec get(CfgName) -> Value
%%     CfgName = atom(),
%%     Value = term()
get(CfgName) ->
    case ets:lookup(?echessd_cfg, CfgName) of
        [{_, Value}] -> Value;
        _ ->
            case default(CfgName) of
                {ok, Default} ->
                    Default;
                _ ->
                    echessd_log:err("'~w' cfg undefined", [CfgName]),
                    throw({cfg_undefined, CfgName})
            end
    end.

%% @doc Return default value for configuration item.
%% @spec default(CfgName) -> {ok, Value} | undefined
%%     CfgName = atom(),
%%     Value = term()
default(?CFG_LOGLEVEL) ->
    {ok, ?LOG_INFO};
default(?CFG_BIND_ADDR) ->
    {ok, {0,0,0,0}};
default(?CFG_BIND_PORT) ->
    {ok, 8888};
default(?CFG_DOC_ROOT) ->
    {ok, "www"};
default(?CFG_LOGFILE) ->
    {ok, "echessd.log"};
default(?CFG_DEF_LANG) ->
    {ok, en};
default(?CFG_DEF_STYLE) ->
    {ok, default};
default(?CFG_HTTPD_MOD) ->
    {ok, echessd_httpd_inets};
default(?CFG_XMPP_ENABLED) ->
    {ok, false};
default(?CFG_XMPP_USER) ->
    {ok, ""};
default(?CFG_XMPP_SERVER) ->
    {ok, ""};
default(?CFG_XMPP_PASSWORD) ->
    {ok, ""};
default(_) ->
    undefined.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

parse_config(String) ->
    try parse_config_(String) of
        {ok, _} = Ok -> Ok;
        {error, _} = Error -> Error;
        Other ->
            {error, {bad_parse_result, Other}}
    catch
        _:{error, _} = ExcError ->
            ExcError;
        Type:Reason ->
            {error, {parse_config, Type, Reason,
                     erlang:get_stacktrace()}}
    end.
parse_config_(String) ->
    put(line, 0),
    Config = parse_config_(String, []),
    lists:foreach(
      fun(Mandatory) ->
              case [K || {K, _} <- Config, K == Mandatory] of
                  [] ->
                      throw(
                        {error,
                         {mandatory_param_undefined, Mandatory}});
                  _ -> nop
              end
      end, ?MANDATORY_CFGS),
    Config.
parse_config_(String, Result) ->
    case get_line_(String, []) of
        eof ->
            {ok, lists:reverse(Result)};
        {Line, Tail} ->
            put(line, erlang:get(line) + 1),
            case parse_line(Line) of
                {_Key, _Value} = Item ->
                    parse_config_(Tail, [Item | Result]);
                _ ->
                    parse_config_(Tail, Result)
            end
    end.

parse_line(Line0) ->
    case echessd_lib:strip(Line0, " \n\r\t") of
        "#" ++ _ ->
            ignore;
        "" ->
            ignore;
        Line ->
            {StrKey, StrVal} = split_kv(Line, []),
            Key = parse_key(StrKey),
            {Key, parse_val(Key, StrVal)}
    end.

parse_key(String) ->
    List = [{atom_to_list(I), I} || I <- ?CFGS],
    case proplists:get_value(String, List) of
        undefined ->
            throw({error,
                   {unknown_key, String,
                    {at_line, erlang:get(line)}}});
        Key -> Key
    end.

parse_val(Key, String) ->
    try parse_val_(Key, String)
    catch
        _:{error, _} = Error ->
            throw(Error);
        _:Reason ->
            throw(
              {error,
               {bad_value, Key, String, Reason,
                {at_line, erlang:get(line)}}})
    end.
parse_val_(?CFG_LOGLEVEL, String0) ->
    String = string:to_lower(String0),
    case proplists:get_value(
           String, [{atom_to_list(I), I} ||  I <- ?LOG_LEVELS]) of
        undefined ->
            throw(
              {error,
               {bad_loglevel, String0,
                {at_line, erlang:get(line)}}});
        LogLevel -> LogLevel
    end;
parse_val_(?CFG_BIND_ADDR, String) ->
    {ok, IP} = inet_parse:address(String),
    IP;
parse_val_(?CFG_BIND_PORT, String) ->
    Int = list_to_integer(String),
    true = Int > 0 andalso Int < 65536,
    Int;
parse_val_(?CFG_DEF_LANG, String) ->
    {LangAbbr, _LangName} = echessd_lib:parse_language(String),
    LangAbbr;
parse_val_(?CFG_DEF_STYLE, String) ->
    {Name, _TextID, _Filename} = echessd_lib:parse_style(String),
    Name;
parse_val_(?CFG_HTTPD_MOD, String0) ->
    String = string:to_lower(String0),
    List = [{atom_to_list(A), A} || A <- ?HTTPD_MODULES],
    case [M || {K, M} <- List,
               K == String orelse
                   K == "echessd_httpd_" ++ String] of
        [Mod | _] -> Mod;
        _ ->
            throw({error, {unsupported_httpd_module, String0}})
    end;
parse_val_(?CFG_XMPP_ENABLED, String0) ->
    String = string:to_lower(String0),
    case lists:member(String, ["yes", "true", "1"]) of
        true -> true;
        _ ->
            case lists:member(String, ["no", "false", "0"]) of
                true -> false;
                _ ->
                    throw({error, {bad_xmpp_enabled_value, String0}})
            end
    end;
parse_val_(_, Val) ->
    Val.

get_line_([$\n | Tail], Line) ->
    {lists:reverse(Line), Tail};
get_line_([H | Tail], Line) ->
    get_line_(Tail, [H | Line]);
get_line_([], [_ | _] = Line) ->
    {lists:reverse(Line), []};
get_line_([], []) -> eof.

split_kv([H | Tail], Key) ->
    case lists:member(H, " \t") of
        true ->
            {string:to_lower(lists:reverse(Key)),
             echessd_lib:strip(Tail, " \t")};
        _ ->
            split_kv(Tail, [H | Key])
    end;
split_kv(_, Key) ->
    {string:to_lower(lists:reverse(Key)), ""}.

read_lang_file() ->
    LangFilename = filename:join("priv", "echessd.lang"),
    case file:consult(LangFilename) of
        {ok, Terms} ->
            Strings =
                [{{ID, Abbr}, S} ||
                    {text, ID, L} <- Terms,
                    {Abbr, S} <- L,
                    is_atom(Abbr), is_list(S)],
            {ok,
             {lists:usort(
                [I || {languages, L} <- Terms,
                      {Abbr, [_ | _]} = I <- L,
                      is_atom(Abbr)]),
              dict:from_list(Strings)}};
        Error -> Error
    end.

read_styles_file() ->
    Filename = filename:join("priv", "echessd.styles"),
    case file:consult(Filename) of
        {ok, Terms} ->
            {ok,
             lists:flatmap(
               fun({style, [_ | _] = PL}) ->
                       [{proplists:get_value(id, PL),
                         proplists:get_value(text_id, PL),
                         proplists:get_value(filename, PL, "")}];
                  (_) -> []
               end, Terms)};
        Error -> Error
    end.

