%%% @doc
%%% Echessd Server main module.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 20 Jan 2012
%%% @copyright 2012, Aleksey Morarash

-module(echessd).

-export(
   [main/1,
    export_database/1,
    import_database/1
   ]).

-include("echessd.hrl").

%% --------------------------------------------------------------------
%% Type definitions
%% --------------------------------------------------------------------

-type parsed_args() :: [parsed_arg()].

-type parsed_arg() ::
        sasl | hup | ping | init | stop |
        export | import | {config, file:filename()}.

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Program entry point.
-spec main(Args :: [string()]) -> no_return().
main([]) ->
    usage();
main(Args) ->
    case lists:member("-h", Args) orelse lists:member("--help", Args) of
        true ->
            usage();
        false ->
            nop
    end,
    %% bind Erlang port mapper only to loopback network interface
    _IgnoredStdout = os:cmd("epmd -address 127.0.0.1 -daemon"),
    %% parse and process command line arguments
    ParsedArgs = parse_args(Args),
    case [O || O <- [hup, ping, stop], lists:member(O, ParsedArgs)] of
        [_, _ | _] ->
            err("--hup, --ping, --stop options are mutually exclusive", []);
        _ ->
            nop
    end,
    case proplists:is_defined(sasl, ParsedArgs) of
        true ->
            ok = application:start(sasl, permanent);
        false ->
            nop
    end,
    ConfigPath = proplists:get_value(config, ParsedArgs),
    Config = echessd_config_parser:read(ConfigPath),
    InstanceID = proplists:get_value(?CFG_CONFIG_PATH, Config),
    Cookie = proplists:get_value(?CFG_COOKIE, Config),
    case proplists:is_defined(hup, ParsedArgs) of
        true ->
            do_hup(InstanceID, Cookie);
        false ->
            nop
    end,
    case proplists:is_defined(ping, ParsedArgs) of
        true ->
            do_ping(InstanceID, Cookie);
        false ->
            nop
    end,
    case proplists:is_defined(stop, ParsedArgs) of
        true ->
            do_stop(InstanceID, Cookie);
        false ->
            nop
    end,
    ok = start_net_kernel(InstanceID, Cookie),
    ok = application:load(?MODULE),
    ok = application:set_env(?MODULE, ?CFG_CONFIG_PATH, ConfigPath),
    ok = application:start(?MODULE, permanent),
    timer:sleep(infinity).

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc Saves all database contents to a file.
%% @spec export_database(Filename) -> ok | {error, Reason}
%%     Filename = string(),
%%     Reason = term()
export_database(Filename) ->
    case echessd_db:dump_users() of
        {ok, UsersData} ->
            case echessd_db:dump_games() of
                {ok, GamesData} ->
                    Terms =
                        [{user, K, V} || {K, V} <- UsersData] ++
                        [{game, K, V} || {K, V} <- GamesData],
                    Timestamp = echessd_lib:timestamp(now()),
                    _Ignored = application:load(?MODULE),
                    AppVersion =
                        case application:get_key(?MODULE, vsn) of
                            {ok, [_ | _] = Version} ->
                                " v." ++ Version;
                            _ -> ""
                        end,
                    echessd_lib:unconsult(
                      Filename,
                      "echessd" ++ AppVersion ++ " database dump\n" ++
                          Timestamp,
                      Terms);
                Error -> Error
            end;
        Error -> Error
    end.

%% @doc Fetches datum from specified file and initiates local
%%      database with them. All previous data will be wiped!
%% @spec import_database(Filename) -> ok | {error, Reason}
%%     Filename = string(),
%%     Reason = term()
import_database(Filename) ->
    case file:consult(Filename) of
        {ok, Terms} ->
            case parse_imported(Terms) of
                {ok, Users, Games} ->
                    ok = echessd_db:init(),
                    ok = echessd_db:wait(),
                    case echessd_db:import_users(Users) of
                        ok ->
                            echessd_db:import_games(Games);
                        Error -> Error
                    end;
                Error -> Error
            end;
        Error -> Error
    end.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

parse_imported(Terms) ->
    try parse_imported_(Terms)
    catch
        _:{error, _Reason} = Error ->
            Error;
        Type:Reason ->
            {error, {Type, Reason, erlang:get_stacktrace()}}
    end.
parse_imported_(Terms) ->
    lists:foldl(
      fun({user, Username, UserInfo}, {ok, Users, Games}) ->
              {ok, [{Username, UserInfo} | Users], Games};
         ({game, GameID, GameInfo}, {ok, Users, Games}) ->
              {ok, Users, [{GameID, GameInfo} | Games]};
         (Other, _Acc) ->
              throw({error, {unknown_import_item, Other}})
      end, {ok, [], []}, Terms).

%% @doc Send reconfig signal to the Echessd Server.
-spec do_hup(InstanceID :: atom(), Cookie :: atom()) -> no_return().
do_hup(InstanceID, Cookie) ->
    MyID = list_to_atom(atom_to_list(InstanceID) ++ "_hupper"),
    ok = start_net_kernel(MyID, Cookie),
    case net_adm:ping(ServerNode = node_fullname(InstanceID)) of
        pong ->
            ok = rpc:call(ServerNode, echessd_cfg, read, []),
            ok = rpc:call(ServerNode, echessd_log, reopen, []),
            ok = rpc:call(ServerNode, echessd_web_warden, reconfig, []),
            halt(0);
        pang ->
            err("Echessd is not alive", [])
    end.

%% @doc Check the Echessd Server.
-spec do_ping(InstanceID :: atom(), Cookie :: atom()) -> no_return().
do_ping(InstanceID, Cookie) ->
    MyID = list_to_atom(atom_to_list(InstanceID) ++ "_pinger"),
    ok = start_net_kernel(MyID, Cookie),
    case net_adm:ping(node_fullname(InstanceID)) of
        pong ->
            ok = io:format("Running~n"),
            halt(0);
        pang ->
            ok = io:format("Not running~n"),
            halt(1)
    end.

%% @doc Tell the Echessd Server to terminate.
-spec do_stop(InstanceID :: atom(), Cookie :: atom()) -> no_return().
do_stop(InstanceID, Cookie) ->
    MyID = list_to_atom(atom_to_list(InstanceID) ++ "_pinger"),
    ok = start_net_kernel(MyID, Cookie),
    case net_adm:ping(ServerNode = node_fullname(InstanceID)) of
        pong ->
            ok = rpc:call(ServerNode, erlang, halt, [0]),
            halt(0);
        pang ->
            err("Echessd is not alive", [])
    end.

%% @doc
-spec start_net_kernel(NodeShortName :: atom(), Cookie :: atom()) ->
                              ok | no_return().
start_net_kernel(NodeShortName, Cookie) ->
    case net_kernel:start([node_fullname(NodeShortName)]) of
        {ok, _Pid} ->
            true = erlang:set_cookie(node(), Cookie),
            ok;
        {error, Reason} ->
            err("Failed to start the Erlang Distribution:~n\t~p", [Reason])
    end.

%% @doc
-spec node_fullname(NodeShortName :: atom()) -> node().
node_fullname(NodeShortName) ->
    list_to_atom(atom_to_list(NodeShortName) ++ "@127.0.0.1").

%% @doc Parse and process command line options and arguments.
-spec parse_args(Args :: [string()]) -> parsed_args() | no_return().
parse_args(Args) ->
    parse_args(Args, []).

-spec parse_args(Args :: [string()], Acc :: parsed_args()) ->
                        parsed_args() | no_return().
parse_args(["--sasl" | Tail], Acc) ->
    parse_args(Tail, [sasl | Acc]);
parse_args(["--hup" | Tail], Acc) ->
    parse_args(Tail, [hup | Acc]);
parse_args(["--ping" | Tail], Acc) ->
    parse_args(Tail, [ping | Acc]);
parse_args(["--stop" | Tail], Acc) ->
    parse_args(Tail, [stop | Acc]);
parse_args(["--", ConfigFilePath], Acc) ->
    [{config, ConfigFilePath} | Acc];
parse_args(["-" ++ _ = Option | _Tail], _Acc) ->
    err("Unrecognized option: ~p", [Option]);
parse_args([ConfigFilePath], Acc) ->
    [{config, ConfigFilePath} | Acc];
parse_args(Other, _Acc) ->
    err("Unrecognized option or arguments: ~p", [Other]).

%% @doc Report something to stderr and halt.
-spec err(Format :: string(), Args :: list()) -> no_return().
err(Format, Args) ->
    ok = io:format(standard_error, "Error: " ++ Format ++ "\n", Args),
    halt(1).

%% @doc
-spec usage() -> no_return().
usage() ->
    N = escript:script_name(),
    io:format(
      "Echessd - web-based Internet Chess Server v.~s~n~n"
      "Usage:~n"
      "  ~s -h | --help~n"
      "\tShow this memo;~n"
      "  ~s /path/to/config~n"
      "\tStart Echessd Server;~n"
      "  ~s --hup /path/to/config~n"
      "\tSend reconfig signal to the running Echessd Server;~n"
      "  ~s --ping /path/to/config~n"
      "\tCheck if the Echessd Server instance is alive or not;~n"
      "  ~s --stop /path/to/config~n"
      "\tTell the Echessd Server to terminate.~n",
      [version(), N, N, N, N, N]),
    halt().

%% @doc Return Echessd version.
-spec version() -> iolist().
version() ->
    case application:load(?MODULE) of
        ok ->
            ok;
        {error, {already_loaded, ?MODULE}} ->
            ok
    end,
    {ok, Version} = application:get_key(?MODULE, vsn),
    Version.
