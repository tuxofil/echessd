%%%-------------------------------------------------------------------
%%% File    : echessd_cfg.erl
%%% Author  : Aleksey Morarash <aleksey.morarash@gmail.com>
%%% Created : 20 Jan 2012
%%% License : FreeBSD
%%% Description : configuration file parsing and configuration
%%%               items accessing tools
%%%-------------------------------------------------------------------

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
                    catch ets:new(?echessd_cfg, [named_table, public, set]),
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
    case strip(Line0, " \n\r\t") of
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
             strip(Tail, " \t")};
        _ ->
            split_kv(Tail, [H | Key])
    end;
split_kv(_, Key) ->
    {string:to_lower(lists:reverse(Key)), ""}.

%% @doc Removes Characters from beginning and ending of String.
%% @spec strip(String, Characters) -> StrippedString
%%     String = Characters = StrippedString = string()
strip(String, Characters) ->
    lists:reverse(
      strip_(
        lists:reverse(
          strip_(String, Characters)),
        Characters)).
strip_([], _Chars) -> [];
strip_([Char | Tail] = String, Chars) ->
    case lists:member(Char, Chars) of
        true ->
            strip_(Tail, Chars);
        _ ->
            String
    end.

