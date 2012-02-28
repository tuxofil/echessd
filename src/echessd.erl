%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 20 Jan 2012
%%% @copyright 2012, Aleksey Morarash
%%% @doc Application start/stop wrapper functions.

-module(echessd).

-export([start/0, stop/0, hup/0, ping/0, stop_remote/0,
         build_doc/0,
         export_database/1,
         import_database/1
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
    ok = rpc:call(Node, echessd_web_warden, reconfig, []),
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
                    application:load(?MODULE),
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

connect_server() ->
    [_, Hostname] = string:tokens(atom_to_list(node()), "@"),
    ServerNode = list_to_atom(?NODE_ECHESSD ++ "@" ++ Hostname),
    pong = net_adm:ping(ServerNode),
    {ok, ServerNode}.

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

