%%% @doc
%%% Echessd configuration file parser.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 22 Nov 2013
%%% @copyright 2013, Aleksey Morarash

-module(echessd_config_parser).

%% API exports
-export([read/1, default/1]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% Type definitions
%% ----------------------------------------------------------------------

-export_type(
   [config/0,
    config_item/0,
    config_key/0,
    loglevel/0
   ]).

-type config() :: [config_item()].

-type config_item() ::
        {?CFG_LOGLEVEL, Loglevel :: loglevel()} |
        {?CFG_LOGFILE, LogFilePath :: nonempty_string() | undefined} |
        {?CFG_BIND_ADDR, BindIP :: inet:ip_address()} |
        {?CFG_BIND_PORT, BindPort :: inet:port_number()} |
        {?CFG_DEF_LANG, DefaultLanguageID :: atom()} |
        {?CFG_DEF_STYLE, DefaultStyleID :: atom()} |
        {?CFG_XMPP_USER, XmppUsername :: string()} |
        {?CFG_XMPP_SERVER, XmppServerHostname :: string()} |
        {?CFG_XMPP_PASSWORD, XmppPassword :: string()} |
        {?CFG_XMPP_ENABLED, XmppNotificationsEnabled :: boolean()} |
        {?CFG_SHOW_ABOUT, ShowAboutInfo :: boolean()} |
        {?CFG_SHOW_COPYRIGHTS, ShowCopyrights :: boolean()} |
        {?CFG_MIME_TYPES, MimeTypesFilePath :: file:filename()} |
        {?CFG_INSTANCE_ID, InstanceID :: atom()} |
        {?CFG_COOKIE, Cookie :: atom()}.

-type config_key() ::
        ?CFG_LOGLEVEL | ?CFG_LOGFILE | ?CFG_BIND_ADDR | ?CFG_BIND_PORT |
        ?CFG_DEF_LANG | ?CFG_DEF_STYLE | ?CFG_XMPP_USER | ?CFG_XMPP_SERVER |
        ?CFG_XMPP_PASSWORD | ?CFG_XMPP_ENABLED | ?CFG_SHOW_ABOUT |
        ?CFG_SHOW_COPYRIGHTS | ?CFG_MIME_TYPES | ?CFG_INSTANCE_ID |
        ?CFG_COOKIE.

-type loglevel() :: ?LOG_ERR | ?LOG_INFO | ?LOG_DEBUG.

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Read and parse the configuration file.
-spec read(ConfigPath :: file:filename()) -> config().
read(ConfigPath) ->
    add_defaults(parse(read_file(ConfigPath))).

%% @doc Return a default value for the configuration key.
-spec default(Key :: config_key()) -> DefaultValue :: any().
default(?CFG_LOGLEVEL) ->
    ?LOG_INFO;
default(?CFG_LOGFILE) ->
    undefined;
default(?CFG_BIND_ADDR) ->
    {0,0,0,0};
default(?CFG_BIND_PORT) ->
    8888;
default(?CFG_DEF_LANG) ->
    en;
default(?CFG_DEF_STYLE) ->
    default;
default(?CFG_XMPP_ENABLED) ->
    false;
default(?CFG_XMPP_USER) ->
    "";
default(?CFG_XMPP_SERVER) ->
    "";
default(?CFG_XMPP_PASSWORD) ->
    "";
default(?CFG_SHOW_ABOUT) ->
    true;
default(?CFG_SHOW_COPYRIGHTS) ->
    true;
default(?CFG_MIME_TYPES) ->
    "/etc/mime.types";
default(?CFG_INSTANCE_ID) ->
    echessd;
default(?CFG_COOKIE) ->
    echessd.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

-spec read_file(ConfigPath :: file:filename()) -> string().
read_file(ConfigPath) ->
    case file:read_file(ConfigPath) of
        {ok, Binary} ->
            binary_to_list(Binary);
        {error, Reason} ->
            echessd_log:err(
              "~w> Failed to read configuration file \"~s\": ~9999p",
              [?MODULE, ConfigPath, Reason]),
            []
    end.

%% @doc Parse the contents of the configuration file
%% to a valid key-value pairs.
-spec parse(ConfigContents :: string()) -> config().
parse(ConfigContents) ->
    lists:flatmap(fun parse_kv/1, preparse(ConfigContents)).

%% @doc Parse the key-value pair.
-spec parse_kv({LineNo :: pos_integer(), StrKey :: nonempty_string(),
                StrValue :: string()}) ->
                      [{Key :: config_key(), Value :: any()}].
parse_kv({LineNo, StrKey, StrValue}) ->
    case echessd_lib:list_to_atom(StrKey, ?CFGS) of
        {ok, Key} ->
            case parse_value(Key, StrValue) of
                {ok, Value} ->
                    {Key, Value};
                error ->
                    echessd_log:err(
                      "~w> Bad value for '~w' at line ~w: ~9999p",
                      [?MODULE, Key, LineNo, StrValue]),
                    error
            end;
        error ->
            echessd_log:err(
              "~w> Unknown configuration key at line ~w: ~9999p",
              [?MODULE, LineNo, StrKey]),
            error
    end.

%% @doc Complete the config with default values for undefined
%% configuration items.
-spec add_defaults(Config :: config()) -> FinalConfig :: config().
add_defaults(Config) ->
    [proplists:get_value(Key, Config, default(Key)) || Key <- ?CFGS].

%% @equiv preparse(String, [{comment_chars, "#%!"}])
%% @doc Make preparse of key-value configuration file.
-spec preparse(String :: string()) ->
                      MeaningLines ::
                        [{LineNo :: pos_integer(),
                          Key :: nonempty_string(),
                          Value :: string()}].
preparse(String) ->
    preparse(String, [{comment_chars, "#%!"}]).

%% @doc Make preparse of key-value configuration file.
-spec preparse(String :: string(),
               Options ::
                 [{comment_chars, Chars :: string()}]) ->
                      MeaningLines ::
                        [{LineNo :: pos_integer(),
                          Key :: nonempty_string(),
                          Value :: string()}].
preparse(String, Options) ->
    CommentChars = proplists:get_value(comment_chars, Options, ""),
    {_LastLineNo, CommentChars, List} =
        lists:foldl(
          fun preparse_loop/2,
          {1, CommentChars, []},
          split_lines(String)),
    lists:reverse(List).

-type preparse_acc() ::
        {LineNo :: pos_integer(),
         CommentChars :: string(),
         ParsedLinesAccumulator :: [{LineNo :: pos_integer(),
                                     Key :: nonempty_string(),
                                     Value :: string()}]}.

%% @doc
-spec preparse_loop(Line :: string(), Acc :: preparse_acc()) ->
                           NewAcc :: preparse_acc().
preparse_loop(Line, {LineNo, CommentChars, Parsed}) ->
    case echessd_lib:strip(Line, " \t\r\n") of
        [] ->
            {LineNo + 1, CommentChars, Parsed};
        [C | _] = Stripped ->
            case lists:member(C, CommentChars) of
                true ->
                    {LineNo + 1, CommentChars, Parsed};
                _ ->
                    case split_to_key_value(Stripped) of
                        {[_ | _] = Key, Value} ->
                            {LineNo + 1, CommentChars,
                             [{LineNo, Key, Value} | Parsed]};
                        _ ->
                            {LineNo + 1, CommentChars, Parsed}
                    end
            end
    end.

%% @doc Split the text to lines. Result lines will be stripped
%% of newline characters.
-spec split_lines(String :: string()) -> Lines :: [string()].
split_lines(String) ->
    split_lines(String, [], []).
split_lines([], [], Lines) ->
    lists:reverse(Lines);
split_lines([], Line, Lines) ->
    split_lines([], [], [lists:reverse(Line) | Lines]);
split_lines("\r\n" ++ Tail, Line, Lines) ->
    split_lines(Tail, [], [lists:reverse(Line) | Lines]);
split_lines("\n" ++ Tail, Line, Lines) ->
    split_lines(Tail, [], [lists:reverse(Line) | Lines]);
split_lines("\r" ++ Tail, Line, Lines) ->
    split_lines(Tail, Line, Lines);
split_lines([C | Tail], Line, Lines) ->
    split_lines(Tail, [C | Line], Lines).

%% @doc Split the line to a key and a value. The first token separated
%% from the others with space characters, will be returned as the key in
%% lower case (so, keys are case insensitive); rest of the line will be
%% returned as the value. Character case of the value will be preserved.
-spec split_to_key_value(String :: string()) ->
                                {Key :: string(), Value :: string()}.
split_to_key_value(String) ->
    split_to_key_value_(echessd_lib:strip(String, " \t\r\n"), []).
split_to_key_value_([], Key) ->
    {string:to_lower(lists:reverse(Key)), []};
split_to_key_value_([C | Tail], Key) ->
    case lists:member(C, " \t") of
        true ->
            {string:to_lower(lists:reverse(Key)),
             echessd_lib:strip(Tail, " \t")};
        _ ->
            split_to_key_value_(Tail, [C | Key])
    end.

%% @doc Parse the value for the given configuration key.
-spec parse_value(Key :: config_key(), String :: string()) ->
                         {ok, Value :: any()} | error.
parse_value(Key, StrValue) ->
    try
        parse_value_(Key, StrValue)
    catch
        _:_ ->
            error
    end.

%% @doc
-spec parse_value_(Key :: config_key(), StrValue :: string()) ->
                          ParsedValue :: any().
parse_value_(?CFG_LOGLEVEL, String) ->
    {ok, Loglevel} = echessd_lib:list_to_atom(String, ?LOG_LEVELS),
    Loglevel;
parse_value_(?CFG_LOGFILE, [_ | _] = String) ->
    String;
parse_value_(?CFG_LOGFILE, []) ->
    undefined;
parse_value_(?CFG_BIND_ADDR, String) ->
    {ok, IP} = inet_parse:address(String),
    IP;
parse_value_(?CFG_BIND_PORT, String) ->
    Int = list_to_integer(String),
    true = Int > 0 andalso Int =< 16#ffff,
    Int;
parse_value_(?CFG_DEF_LANG, String) ->
    Langs = [LangID || {LangID, _LangName} <- echessd_lang:list()],
    {ok, LangID} = echessd_lib:list_to_atom(String, Langs),
    LangID;
parse_value_(?CFG_DEF_STYLE, String) ->
    Styles = [StyleID || {StyleID, _TextID} <- echessd_styles:list()],
    {ok, StyleID} = echessd_lib:list_to_atom(String, Styles),
    StyleID;
parse_value_(?CFG_XMPP_ENABLED, String) ->
    {ok, Boolean} = parse_boolean(String),
    Boolean;
parse_value_(?CFG_XMPP_USER, String) ->
    String;
parse_value_(?CFG_XMPP_SERVER, String) ->
    String;
parse_value_(?CFG_XMPP_PASSWORD, String) ->
    String;
parse_value_(?CFG_SHOW_ABOUT, String) ->
    {ok, Boolean} = parse_boolean(String),
    Boolean;
parse_value_(?CFG_SHOW_COPYRIGHTS, String) ->
    {ok, Boolean} = parse_boolean(String),
    Boolean;
parse_value_(?CFG_MIME_TYPES, String) ->
    String;
parse_value_(?CFG_INSTANCE_ID, String) ->
    list_to_atom(String);
parse_value_(?CFG_COOKIE, String) ->
    list_to_atom(String).

%% @doc
-spec parse_boolean(String :: string()) -> {ok, boolean()} | error.
parse_boolean(String) ->
    Lowered = string:to_lower(String),
    case lists:member(Lowered, ["y", "yes", "t", "true", "1"]) of
        true ->
            {ok, true};
        _ ->
            case lists:member(Lowered, ["n", "no", "f", "false", "0"]) of
                true ->
                    {ok, false};
                _ ->
                    error
            end
    end.
