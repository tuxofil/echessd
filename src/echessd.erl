%%% @doc
%%% Echessd Server main module.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 20 Jan 2012
%%% @copyright 2012, Aleksey Morarash

-module(echessd).

%% API exports
-export([main/1]).

-include("echessd.hrl").

%% --------------------------------------------------------------------
%% Type definitions
%% --------------------------------------------------------------------

-type parsed_args() :: [parsed_arg()].

-type parsed_arg() ::
        sasl | hup | ping | init | stop |
        dump | load | {config, file:filename()}.

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
    %% parse command line arguments
    ParsedArgs = parse_args(Args),
    case proplists:is_defined(sasl, ParsedArgs) of
        true ->
            ok = application:start(sasl, permanent);
        false ->
            nop
    end,
    %% read and parse configuration file
    ConfigPath = proplists:get_value(config, ParsedArgs),
    Config = echessd_config_parser:read(ConfigPath),
    InstanceID = proplists:get_value(?CFG_INSTANCE_ID, Config),
    Cookie     = proplists:get_value(?CFG_COOKIE, Config),
    MnesiaDir  = proplists:get_value(?CFG_DB_PATH, Config),
    %% process command line arguments
    ok = handle_cmd_args(ParsedArgs, InstanceID, Cookie, MnesiaDir),
    ok = start_net_kernel(InstanceID, Cookie),
    ok = application:load(mnesia),
    ok = application:set_env(mnesia, dir, MnesiaDir),
    case is_db_present(MnesiaDir) of
        true ->
            ok = application:start(mnesia, permanent);
        false ->
            %% autocreate mnesia database
            ok = echessd_db:init()
    end,
    ok = application:load(?MODULE),
    ok = application:set_env(?MODULE, ?CFG_CONFIG_PATH, ConfigPath),
    ok = application:start(?MODULE, permanent),
    timer:sleep(infinity).

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc Handle command line arguments (options) one by one.
-spec handle_cmd_args(ParsedArgs :: parsed_args(),
                      InstanceID :: atom(),
                      Cookie :: atom(),
                      MnesiaDir :: nonempty_string()) ->
                             ok | no_return().
handle_cmd_args(ParsedArgs, InstanceID, Cookie, MnesiaDir) ->
    case [O || O <- [hup, ping, stop, init, dump, load],
               proplists:is_defined(O, ParsedArgs)] of
        [_, _ | _] ->
            err("--hup, --ping, --stop, --init, --dump and --load "
                "options are mutually exclusive", []);
        _ ->
            nop
    end,
    lists:foreach(
      fun(ParsedArg) ->
              handle_cmd_arg(ParsedArg, InstanceID, Cookie, MnesiaDir)
      end, ParsedArgs).

%% @doc Helper for the handle_cmd_args/1 fun.
-spec handle_cmd_arg(ParsedArg :: parsed_arg(),
                     InstanceID :: atom(),
                     Cookie :: atom(),
                     MnesiaDir :: nonempty_string()) ->
                            ok | no_return().
handle_cmd_arg(init, InstanceID, _Cookie, MnesiaDir) ->
    do_init(InstanceID, MnesiaDir);
handle_cmd_arg(hup, InstanceID, Cookie, _MnesiaDir) ->
    do_hup(InstanceID, Cookie);
handle_cmd_arg(ping, InstanceID, Cookie, _MnesiaDir) ->
    do_ping(InstanceID, Cookie);
handle_cmd_arg(stop, InstanceID, Cookie, _MnesiaDir) ->
    do_stop(InstanceID, Cookie);
handle_cmd_arg({dump, DumpFilePath}, InstanceID, Cookie, _MnesiaDir) ->
    do_dump(InstanceID, Cookie, DumpFilePath);
handle_cmd_arg({load, DumpFilePath}, InstanceID, Cookie, _MnesiaDir) ->
    do_load(InstanceID, Cookie, DumpFilePath);
handle_cmd_arg(_ParsedArg, _InstanceID, _Cookie, _MnesiaDir) ->
    ok.

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

%% @doc Initialize the Echessd database.
-spec do_init(InstanceID :: atom(), MnesiaDir :: nonempty_string()) ->
                     no_return().
do_init(InstanceID, MnesiaDir) ->
    ok = start_net_kernel(InstanceID, _Cookie = no_matter),
    ok = application:load(mnesia),
    ok = application:set_env(mnesia, dir, MnesiaDir),
    ok = echessd_db:init(),
    halt(0).

%% @doc Make a dump of the Echessd database to a file.
-spec do_dump(InstanceID :: atom(), Cookie :: atom(),
              DumpFilePath :: nonempty_string()) -> no_return().
do_dump(InstanceID, Cookie, DumpFilePath) ->
    MyID = list_to_atom(atom_to_list(InstanceID) ++ "_dumper"),
    ok = start_net_kernel(MyID, Cookie),
    case net_adm:ping(ServerNode = node_fullname(InstanceID)) of
        pong ->
            ok = echessd_lib:unconsult(
                   DumpFilePath,
                   ["echessd v.", version(), " database dump\n",
                    echessd_lib:timestamp(now())],
                   rpc:call(ServerNode, echessd_db, dump, [])),
            halt(0);
        pang ->
            err("Echessd is not alive", [])
    end.

%% @doc Initialize the database from the dump file.
-spec do_load(InstanceID :: atom(), Cookie :: atom(),
              DumpFilePath :: nonempty_string()) -> no_return().
do_load(InstanceID, Cookie, DumpFilePath) ->
    MyID = list_to_atom(atom_to_list(InstanceID) ++ "_loader"),
    ok = start_net_kernel(MyID, Cookie),
    case net_adm:ping(ServerNode = node_fullname(InstanceID)) of
        pong ->
            {ok, Dump} = file:consult(DumpFilePath),
            ok = rpc:call(ServerNode, echessd_db, load, [Dump]),
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
parse_args(["--init" | Tail], Acc) ->
    parse_args(Tail, [init | Acc]);
parse_args(["--dump", [_ | _] = DumpFilePath | Tail], Acc) ->
    parse_args(Tail, [{dump, DumpFilePath} | Acc]);
parse_args(["--dump", _ | _Tail], _Acc) ->
    err("--dump option assumes non empty value", []);
parse_args(["--load", [_ | _] = DumpFilePath | Tail], Acc) ->
    parse_args(Tail, [{load, DumpFilePath} | Acc]);
parse_args(["--load", _ | _Tail], _Acc) ->
    err("--load option assumes non empty value", []);
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
      "\tTell the Echessd Server to terminate;~n"
      "  ~s --init /path/to/config~n"
      "\tInitialize the Echessd database. "
      "Warning: all existing data will be lost!~n"
      "  ~s --dump DumpFilePath /path/to/config~n"
      "\tCreate a dump of the database to a file;~n"
      "  ~s --load DumpFilePath /path/to/config~n"
      "\tRead the dump file and initialize the database with the "
      "dump data.~n\tWarning: all existing data will be lost!~n",
      [version(), N, N, N, N, N, N, N, N]),
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

%% @doc Return false if configured location for mnesia database
%% is not exist or does not contain any files.
-spec is_db_present(MnesiaDir :: nonempty_string()) ->
                           boolean().
is_db_present(MnesiaDir) ->
    case filelib:is_dir(MnesiaDir) of
        true ->
            lists:all(
              fun(MandatoryFile) ->
                      filelib:is_file(
                        filename:join(MnesiaDir, MandatoryFile))
              end, ["LATEST.LOG", "schema.DAT"]);
        false ->
            false
    end.
