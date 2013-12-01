%%% @doc
%%% Interface to the Echessd persistent storage.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 21 Jan 2012
%%% @copyright 2012, Aleksey Morarash

-module(echessd_db).

-export(
   [init/0, wait/0,
    list_users/0,
    adduser/2,
    deluser/1,
    get_user_props/1,
    set_user_props/2,
    addgame/1,
    game_ack/2,
    game_deny/2,
    get_game_props/1,
    set_game_props/2,
    delgame/1,
    gameply/3,
    game_give_up/2,
    game_request_draw/2,
    gamerewind/1,
    gamereset/1,
    dump_users/0,
    dump_games/0,
    dump/0,
    import_users/1,
    import_games/1,
    load/1
   ]).

-include("echessd.hrl").

%% database tables
-define(dbt_users, echessd_dbt_users).
-define(dbt_games, echessd_dbt_games).

%% db table record
-record(hrec, {key, val}).

%% ----------------------------------------------------------------------
%% Type definitions
%% ----------------------------------------------------------------------

-type transaction() :: fun(() -> Result :: any()).

-type db_table() :: ?dbt_users | ?dbt_games.

-type proplist() :: [{Key :: atom(), Value :: any()}].

-type rec_id() :: echessd_game:id() | echessd_user:name() | counter.

-type dump_item() ::
        {user, UserName :: echessd_user:name(),
         UserInfo :: echessd_user:info()} |
        {game, GameID :: echessd_game:id(),
         GameInfo :: echessd_game:info()}.

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Create mnesia database from scratch.
%% All previous data if will be wiped off.
%% Normally the function is called only once before the first server start.
-spec init() -> ok.
init() ->
    stopped = mnesia:stop(),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    DbOpts =
        [{type, set},
         {record_name, hrec},
         {disc_copies, [node()]}],
    {atomic, ok} =
        mnesia:create_table(?dbt_users, DbOpts),
    {atomic, ok} =
        mnesia:create_table(?dbt_games, DbOpts),
    ok.

%% @doc Wait until all database tables become available
%% or fail if a timeout of 10 seconds elapsed.
%% Called at the server start from the application supervisor init/0.
-spec wait() -> ok.
wait() ->
    case mnesia:wait_for_tables(
           [?dbt_users, ?dbt_games], 10000) of
        ok -> ok;
        {timeout, BadTabList} = Error ->
            echessd_log:err(
              "Timeout waiting DB tables: ", [BadTabList]),
            throw(Error);
        {error, Reason} ->
            echessd_log:err(
              "Error occured while waiting DB tables: ", [Reason]),
            throw(Reason)
    end.

%% @doc Return a list of all registered user names.
-spec list_users() -> {ok, Users :: [echessd_user:name()]} |
                      {error, Reason :: any()}.
list_users() ->
    transaction(
      fun() ->
              mnesia:foldl(
                fun(HRec, Acc) ->
                        case echessd_user:get_value(
                               show_in_list, HRec#hrec.val) of
                            false -> Acc;
                            _ ->
                                [HRec#hrec.key | Acc]
                        end
                end, [], ?dbt_users)
      end).

%% @doc Add a new user.
-spec adduser(Username :: echessd_user:name(),
              UserInfo :: echessd_user:info()) ->
                     ok | {error, Reason :: any()}.
adduser(Username, UserInfo) ->
    transaction_ok(
      fun() ->
              case mnesia:read({?dbt_users, Username}) of
                  [_] ->
                      mnesia:abort({user_already_exists, Username});
                  _ ->
                      ll_set_props(?dbt_users, Username, UserInfo)
              end
      end).

%% @doc Remove the user.
-spec deluser(Username :: echessd_user:name()) ->
                     ok | {error, Reason :: any()}.
deluser(Username) ->
    transaction_ok(
      fun() ->
              mnesia:delete({?dbt_users, Username})
      end).

%% @doc Fetch the user properties.
-spec get_user_props(Username :: echessd_user:name()) ->
                            {ok, UserInfo :: echessd_user:info()} |
                            {error, Reason :: any()}.
get_user_props(Username) ->
    transaction(
      fun() ->
              [{login, Username} |
               ll_get_props(?dbt_users, Username)]
      end).

%% @doc Set the user properties.
-spec set_user_props(Username :: echessd_user:name(),
                     UserInfo :: echessd_user:info()) ->
                            ok | {error, Reason :: any()}.
set_user_props(Username, UserInfo) ->
    transaction_ok(
      fun() ->
              OldProps = ll_get_props(?dbt_users, Username),
              ll_replace_props(
                ?dbt_users, Username, OldProps, UserInfo)
      end).

%% @doc Add a new game to the database.
-spec addgame(GameInfo :: echessd_game:info()) ->
                     {ok, GameID :: echessd_game:id()} |
                     {error, Reason :: any()}.
addgame(GameInfo) ->
    transaction(
      fun() ->
              GameID = generate_game_id(),
              set_game_props_(GameID, GameInfo),
              GameID
      end).

%% @doc Acknowledge the game.
-spec game_ack(GameID :: echessd_game:id(),
               Username :: echessd_user:name()) ->
                      ok | {error, Reason :: any()}.
game_ack(GameID, Username) ->
    transaction_ok(
      fun() ->
              _UserInfo = ll_get_props(?dbt_users, Username),
              GameInfo = ll_get_props(?dbt_games, GameID),
              case proplists:get_value(creator, GameInfo) of
                  Username ->
                      mnesia:abort(not_your_game);
                  _ -> nop
              end,
              List =
                  [z || {users, [_ | _] = L} <- GameInfo,
                        {N, C} <- L,
                        N == Username,
                        lists:member(C, [?white, ?black])],
              if length(List) > 0 -> nop;
                 true ->
                      mnesia:abort(not_your_game)
              end,
              case proplists:get_value(acknowledged, GameInfo) of
                  true -> ok;
                  _ ->
                      ll_replace_props(
                        ?dbt_games, GameID, GameInfo,
                        [{acknowledged, true}])
              end
      end).

%% @doc Deny not confirmed game.
-spec game_deny(GameID :: echessd_game:id(),
                Username :: echessd_user:name()) ->
                       ok | {error, Reason :: any()}.
game_deny(GameID, Username) ->
    transaction_ok(
      fun() ->
              _UserInfo = ll_get_props(?dbt_users, Username),
              GameInfo = ll_get_props(?dbt_games, GameID),
              List =
                  [z || {users, [_ | _] = L} <- GameInfo,
                        {N, C} <- L,
                        N == Username,
                        lists:member(C, [?white, ?black])],
              if length(List) > 0 -> nop;
                 true ->
                      mnesia:abort(not_your_game)
              end,
              case proplists:get_value(acknowledged, GameInfo) of
                  true ->
                      mnesia:abort(unable_to_deny_confirmed_game);
                  _ ->
                      delgame_(GameID, GameInfo)
              end
      end).

%% @doc Fetch the game properties.
-spec get_game_props(GameID :: echessd_game:id()) ->
                            {ok, GameInfo :: echessd_game:info()} |
                            {error, Reason :: any()}.
get_game_props(GameID) ->
    transaction(
      fun() ->
              ll_get_props(?dbt_games, GameID)
      end).

%% @doc Set the game properties.
-spec set_game_props(GameID :: echessd_game:id(),
                     GameInfo :: echessd_game:info()) ->
                            ok | {error, Reason :: any()}.
set_game_props(GameID, GameInfo) ->
    transaction_ok(
      fun() ->
              OldInfo = ll_get_props(?dbt_games, GameID),
              NewInfo = proplist_update(OldInfo, GameInfo),
              set_game_props_(GameID, NewInfo)
      end).

set_game_props_(GameID, GameInfo) ->
    Users = [N || {users, L} <- GameInfo, {N, _Role} <- L],
    lists:foreach(
      fun(Username) ->
              UserInfo = ll_get_props(?dbt_users, Username),
              UserGames =
                  lists:usort(
                    [GameID | echessd_user:get_value(games, UserInfo)]),
              ll_replace_props(
                ?dbt_users, Username, UserInfo,
                [{games, UserGames}])
      end, Users),
    ll_set_props(?dbt_games, GameID, GameInfo).

%% @doc Remove the game from the database.
-spec delgame(GameID :: echessd_game:id()) ->
                     ok | {error, Reason :: any()}.
delgame(GameID) ->
    transaction_ok(
      fun() ->
              case mnesia:read({?dbt_games, GameID}) of
                  [HRec] ->
                      GameInfo = HRec#hrec.val,
                      delgame_(GameID, GameInfo);
                  _ -> ok
              end
      end).

delgame_(GameID, GameInfo) ->
    Users = [N || {users, L} <- GameInfo, {N, _Role} <- L],
    lists:foreach(
      fun(Username) ->
              UserInfo = ll_get_props(?dbt_users, Username),
              UserGames =
                  lists:usort(
                    echessd_user:get_value(
                      games, UserInfo) -- [GameID]),
              ll_replace_props(
                ?dbt_users, Username, UserInfo,
                [{games, UserGames}])
      end, Users),
    mnesia:delete({?dbt_games, GameID}).

%% @doc Make the user turn.
-spec gameply(GameID :: echessd_game:id(),
              Username :: echessd_user:name(),
              Ply :: echessd_game:ply()) ->
                     {ok, GameInfo :: echessd_game:info()} |
                     {error, Reason :: any()}.
gameply(GameID, Username, Ply) ->
    transaction(
      fun() ->
              GameInfo = ll_get_props(?dbt_games, GameID),
              case proplists:get_value(acknowledged, GameInfo) of
                  true -> nop;
                  _ ->
                      mnesia:abort(game_not_acknowledged)
              end,
              %% check if such user exists
              _UserInfo = ll_get_props(?dbt_users, Username),
              case proplists:get_value(status, GameInfo) of
                  alive -> nop;
                  _ ->
                      mnesia:abort(game_ended)
              end,
              case echessd_game:who_must_turn(GameInfo) of
                  Username ->
                      TurnColor = echessd_game:turn_color(GameInfo),
                      GameType = proplists:get_value(type, GameInfo),
                      History = proplists:get_value(moves, GameInfo, []),
                      {ok, _NewBoard, NewHistory, GameStatus} =
                          echessd_game:move(GameType, History, Ply),
                      {Winner, WinnerColor} =
                          case GameStatus of
                              checkmate ->
                                  {Username, TurnColor};
                              _ ->
                                  {undefined, undefined}
                          end,
                      NewGameInfo =
                          proplist_update(
                            GameInfo,
                            [{moves, NewHistory},
                             {status, GameStatus},
                             {winner, Winner},
                             {winner_color, WinnerColor}]),
                      ll_set_props(?dbt_games, GameID, NewGameInfo),
                      NewGameInfo;
                  _ ->
                      case lists:member(
                             Username,
                             [N || {users, L} <- GameInfo,
                                   {N, Role} <- L,
                                   lists:member(Role, [?white, ?black])]) of
                          true ->
                              mnesia:abort(not_your_turn);
                          _ ->
                              mnesia:abort(not_your_game)
                      end
              end
      end).

%% @doc Make the user to fail the game by giving up.
-spec game_give_up(GameID :: echessd_game:id(),
                   Username :: echessd_user:name()) ->
                          ok | {error, Reason :: any()}.
game_give_up(GameID, Username) ->
    transaction_ok(
      fun() ->
              GameInfo = ll_get_props(?dbt_games, GameID),
              case proplists:get_value(acknowledged, GameInfo) of
                  true -> nop;
                  _ ->
                      mnesia:abort(game_not_acknowledged)
              end,
              %% check if such user exists
              _UserInfo = ll_get_props(?dbt_users, Username),
              Players =
                  [I || {users, L} <- GameInfo,
                        {_N, R} = I <- L,
                        lists:member(R, [?white, ?black])],
              case [N || {N, _} <- Players, N == Username] of
                  [_ | _] -> nop;
                  _ ->
                      mnesia:abort(not_your_game)
              end,
              MyColor =
                  case lists:usort([N || {N, _} <- Players]) of
                      [_] ->
                          echessd_game:turn_color(GameInfo);
                      _ ->
                          [MyColor0 | _] =
                              [C || {N, C} <- Players, N == Username],
                          MyColor0
                  end,
              WinnerColor = hd([?white, ?black] -- [MyColor]),
              [Winner | _] =
                  [N || {N, C} <- Players, C == WinnerColor],
              case proplists:get_value(status, GameInfo) of
                  alive ->
                      ll_replace_props(
                        ?dbt_games, GameID, GameInfo,
                        [{status, give_up},
                         {winner, Winner},
                         {winner_color, WinnerColor}]);
                  give_up -> ok;
                  _ ->
                      mnesia:abort(game_ended)
              end
      end).

%% @doc Make a draw request.
-spec game_request_draw(GameID :: echessd_game:id(),
                        Username :: echessd_user:name()) ->
                               ok | {error, Reason :: any()}.
game_request_draw(GameID, Username) ->
    transaction_ok(
      fun() ->
              GameInfo = ll_get_props(?dbt_games, GameID),
              case proplists:get_value(acknowledged, GameInfo) of
                  true -> nop;
                  _ ->
                      mnesia:abort(game_not_acknowledged)
              end,
              %% check if such user exists
              _UserInfo = ll_get_props(?dbt_users, Username),
              Players =
                  [I || {users, L} <- GameInfo,
                        {_N, R} = I <- L,
                        lists:member(R, [?white, ?black])],
              case [N || {N, _} <- Players, N == Username] of
                  [_ | _] -> nop;
                  _ ->
                      mnesia:abort(not_your_game)
              end,
              MyColor =
                  case lists:usort([N || {N, _} <- Players]) of
                      [_] ->
                          echessd_game:turn_color(GameInfo);
                      _ ->
                          [MyColor0 | _] =
                              [C || {N, C} <- Players, N == Username],
                          MyColor0
                  end,
              [Opponent | _] = [N || {N, C} <- Players, C /= MyColor],
              case proplists:get_value(status, GameInfo) of
                  alive ->
                      OldDrawRequest =
                          proplists:get_value(
                            draw_request_from, GameInfo),
                      if Username == Opponent ->
                              %% test game: just end the game
                              ll_replace_props(
                                ?dbt_games, GameID, GameInfo,
                                [{status, {draw, agreement}},
                                 {winner, undefined},
                                 {winner_color, undefined}]);
                         OldDrawRequest == Opponent ->
                              %% confirm draw
                              ll_replace_props(
                                ?dbt_games, GameID, GameInfo,
                                [{status, {draw, agreement}},
                                 {winner, undefined},
                                 {winner_color, undefined}]);
                         true ->
                              %% make draw request
                              ll_replace_props(
                                ?dbt_games, GameID, GameInfo,
                                [{draw_request_from, Username},
                                 {winner, undefined},
                                 {winner_color, undefined}])
                      end;
                  _ ->
                      mnesia:abort(game_ended)
              end
      end).

%% @doc Cancel the last ply for the game.
-spec gamerewind(GameID :: echessd_game:id()) ->
                        ok | {error, Reason :: any()}.
gamerewind(GameID) ->
    transaction_ok(
      fun() ->
              GameInfo = ll_get_props(?dbt_games, GameID),
              History = proplists:get_value(moves, GameInfo, []),
              NewHistory = all_except_last(History),
              ll_replace_props(
                ?dbt_games, GameID, GameInfo,
                [{moves, NewHistory}, {status, alive},
                 {winner, undefined},
                 {winner_color, undefined}])
      end).

%% @doc Cancel all plies for the game.
-spec gamereset(GameID :: echessd_game:id()) ->
                       ok | {error, Reason :: any()}.
gamereset(GameID) ->
    transaction_ok(
      fun() ->
              GameInfo = ll_get_props(?dbt_games, GameID),
              ll_replace_props(
                ?dbt_games, GameID, GameInfo,
                [{moves, []}, {status, alive},
                 {winner, undefined},
                 {winner_color, undefined}])
      end).

%% @doc Make a dump of all user accounts from the database.
-spec dump_users() -> {ok, [{Username :: echessd_user:name(),
                             UserInfo :: echessd_user:info()}]} |
                      {error, Reason :: any()}.
dump_users() ->
    transaction(
      fun() ->
              mnesia:foldl(
                fun(HRec, Acc) ->
                        [{HRec#hrec.key, HRec#hrec.val} | Acc]
                end, [], ?dbt_users)
      end).

%% @doc Make a dump of all games from the database.
-spec dump_games() -> {ok, [{GameID :: echessd_game:id(),
                             GameInfo :: echessd_game:info()}]} |
                      {error, Reason :: any()}.
dump_games() ->
    transaction(
      fun() ->
              mnesia:foldl(
                fun(HRec, Acc) ->
                        [{HRec#hrec.key, HRec#hrec.val} | Acc]
                end, [], ?dbt_games)
      end).

%% @doc Make a dump of all users and games from the database.
-spec dump() -> Dump :: [dump_item()].
dump() ->
    {ok, UserDump} = dump_users(),
    {ok, GameDump} = dump_games(),
    [{user, UserName, UserInfo} || {UserName, UserInfo} <- UserDump] ++
        [{game, GameID, GameInfo} || {GameID, GameInfo} <- GameDump].

%% @doc Insert the user accounts directly to the database.
%% The function intended only for an import operations.
-spec import_users(Users :: [{Username :: echessd_user:name(),
                              UserInfo :: echessd_user:info()}]) ->
                          ok | {error, Reason :: any()}.
import_users(Users) ->
    transaction_ok(
      fun() ->
              lists:foreach(
                fun({Username, UserInfo}) ->
                        ll_set_props(?dbt_users, Username, UserInfo)
                end, Users)
      end).

%% @doc Insert the games directly to the database.
%% The function intended only for an import operations.
-spec import_games(Games :: [{GameID :: echessd_game:id(),
                              GameInfo :: echessd_game:info()}]) ->
                          ok | {error, Reason :: any()}.
import_games(Games) ->
    transaction_ok(
      fun() ->
              lists:foreach(
                fun({GameID, GameInfo}) ->
                        ll_set_props(?dbt_games, GameID, GameInfo)
                end, Games)
      end).

%% @doc Initialize the database from the dump.
-spec load(list()) -> ok.
load(Dump) ->
    ok = init(),
    ok = wait(),
    ok = import_users([{N, I} || {user, N, I} <- Dump]),
    ok = import_games([{N, I} || {game, N, I} <- Dump]).

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc
-spec proplist_update(Subject :: proplist(), NewData :: proplist()) ->
                             UpdatedSubject :: proplist().
proplist_update(Subject, []) ->
    Subject;
proplist_update(Subject, [{Key, NewValue} | Tail]) ->
    proplist_update(
      [{Key, NewValue} | [I || {K, _} = I <- Subject, K /= Key]],
      Tail).

%% @doc
-spec all_except_last(List :: list()) -> NewList :: list().
all_except_last([]) ->
    [];
all_except_last([_Last]) ->
    [];
all_except_last([H | Tail]) ->
    [H | all_except_last(Tail)].

%% @doc
-spec transaction_ok(Fun :: transaction()) ->
                            ok | {error, Reason :: any()}.
transaction_ok(Fun) ->
    case transaction(Fun) of
        {ok, _IgnoredResult} ->
            ok;
        {error, _Reason} = Error ->
            Error
    end.

%% @doc
-spec transaction(Fun :: transaction()) ->
                         {ok, Result :: any()} |
                         {error, Reason :: any()}.
transaction(Fun) ->
    case mnesia:transaction(Fun) of
        {atomic, Result} ->
            {ok, Result};
        {aborted, Reason} ->
            {error, Reason}
    end.

%% @doc
-spec ll_replace_props(DbTable :: db_table(), RecID :: rec_id(),
                       OldProps :: proplist(),
                       Props2Replace :: proplist()) -> ok.
ll_replace_props(DbTable, RecID, OldProps, Props2Replace) ->
    ll_set_props(
      DbTable, RecID, proplist_update(OldProps, Props2Replace)).

%% @doc
-spec ll_get_props(DbTable :: db_table(), RecID :: rec_id()) ->
                          Value :: proplist().
ll_get_props(DbTable, RecID) ->
    case mnesia:read({DbTable, RecID}) of
        [HRec] ->
            HRec#hrec.val;
        _ when DbTable == ?dbt_users ->
            mnesia:abort({no_such_user, RecID});
        _ when DbTable == ?dbt_games ->
            mnesia:abort({no_such_game, RecID});
        _ ->
            mnesia:abort({no_such_item, RecID})
    end.

%% @doc
-spec ll_set_props(DbTable :: db_table(), RecID :: rec_id(),
                   Props :: proplist()) -> ok.
ll_set_props(DbTable, RecID, Props) ->
    mnesia:write(
      DbTable,
      #hrec{key = RecID,
            val = Props},
      write).

%% @doc
-spec generate_game_id() -> echessd_game:id().
generate_game_id() ->
    GameID =
        case mnesia:read({?dbt_games, counter}) of
            [HRec] ->
                HRec#hrec.val;
            _ ->
                1
        end,
    mnesia:write(
      ?dbt_games, #hrec{key = counter, val = GameID + 1},
      write),
    GameID.
