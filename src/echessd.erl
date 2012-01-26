%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 20 Jan 2012
%%% @copyright 2012, Aleksey Morarash
%%% @doc Application start/stop wrapper functions.

-module(echessd).

-export([start/0, stop/0, hup/0, ping/0, stop_remote/0,
         build_doc/0
        ]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Load and start echessd.
%% @spec start() -> ok | {error, Reason}
%%     Reason = term()
start() ->
    application:start(?MODULE, permanent).

%% @doc Stop echessd.
%% @spec stop() -> ok | {error, Reason}
%%     Reason = term()
stop() ->
    case application:stop(?MODULE) of
        {error, {not_started, ?MODULE}} ->
            ok;
        Other ->
            Other
    end.

%% @doc Connects to Erlang node with echessd running and makes
%%      him re-read its configuration file and reopen log file.
%% @spec hup() -> no_return()
hup() ->
    {ok, Node} = connect_server(),
    ok = rpc:call(Node, echessd_cfg, read, []),
    ok = rpc:call(Node, echessd_log, reopen, []),
    halt(0).

%% @doc Checks if Erlang node with running echessd exists.
%% @spec ping() -> no_return()
ping() ->
    {ok, Node} = connect_server(),
    Apps = rpc:call(Node, application, which_applications, []),
    case [?MODULE || {?MODULE, _, _} <- Apps] of
        [_] -> halt(0);
        _ -> halt(1)
    end.

%% @doc Stops echessd server on remote Erlang node.
%% @spec stop_remote() -> no_return()
stop_remote() ->
    {ok, Node} = connect_server(),
    ok = rpc:call(Node, ?MODULE, stop, []),
    catch rpc:call(Node, init, stop, []),
    halt(0).

%% @doc Builds echessd developer documentation from sources.
%%      This function needed only on build stage.
%% @spec build_doc() -> no_return()
build_doc() ->
    edoc:application(?MODULE, ".", []),
    halt(0).

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

connect_server() ->
    [_, Hostname] = string:tokens(atom_to_list(node()), "@"),
    ServerNode = list_to_atom(?NODE_ECHESSD ++ "@" ++ Hostname),
    pong = net_adm:ping(ServerNode),
    {ok, ServerNode}.

