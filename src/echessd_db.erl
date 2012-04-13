%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 21 Jan 2012
%%% @copyright 2012, Aleksey Morarash
%%% @doc Persistent storage interface.

-module(echessd_db).

-export([init/0, wait/0,
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
         import_users/1,
         import_games/1
        ]).

-include("echessd.hrl").

%% db table record
-record(hrec, {key, val}).

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Creates mnesia database from scratch. All previous data if
%%      such exists will be wiped off.
%%      Normally this function called from 'init.sh' script only
%%      once before first server start.
%% @spec init() -> ok
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

%% @doc Waits until all needed database tables becomes available
%%      or fails if timeout of 10 seconds reaches faster.
%%      Called at server start from application supervisor init/0.
%% @spec wait() -> ok
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

%% @doc Fetch registered users names.
%% @spec list_users() -> {ok, Users} | {error, Reason}
%%     Users = [echessd_user:echessd_user()],
%%     Reason = term()
list_users() ->
    transaction(
      fun() ->
              mnesia:foldl(
                fun(HRec, Acc) ->
                        case proplists:get_value(
                               show_in_list, HRec#hrec.val) of
                            false -> Acc;
                            _ ->
                                [HRec#hrec.key | Acc]
                        end
                end, [], ?dbt_users)
      end).

%% @doc Adds new user.
%% @spec adduser(Username, UserInfo) -> ok | {error, Reason}
%%     Username = echessd_user:echessd_user(),
%%     UserInfo = echessd_user:echessd_user_info(),
%%     Reason = term()
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

%% @doc Removes existing user.
%% @spec deluser(Username) -> ok | {error, Reason}
%%     Username = echessd_user:echessd_user(),
%%     Reason = term()
deluser(Username) ->
    transaction_ok(
      fun() ->
              mnesia:delete({?dbt_users, Username})
      end).

%% @doc Fetch user properties.
%% @spec get_user_props(Username) -> {ok, UserInfo} | {error, Reason}
%%     Username = echessd_user:echessd_user(),
%%     UserInfo = echessd_user:echessd_user_info(),
%%     Reason = term()
get_user_props(Username) ->
    transaction(
      fun() ->
              [{login, Username} |
               ll_get_props(?dbt_users, Username)]
      end).

%% @doc Set user properties.
%% @spec set_user_props(Username, UserInfo) -> ok | {error, Reason}
%%     Username = echessd_user:echessd_user(),
%%     UserInfo = echessd_user:echessd_user_info(),
%%     Reason = term()
set_user_props(Username, UserInfo) ->
    transaction_ok(
      fun() ->
              OldProps = ll_get_props(?dbt_users, Username),
              ll_replace_props(
                ?dbt_users, Username, OldProps, UserInfo)
      end).

%% @doc Adds new game to database.
%% @spec addgame(GameInfo) -> {ok, GameID} | {error, Reason}
%%     GameInfo = echessd_game:echessd_game_info(),
%%     GameID = echessd_game:echessd_game_id(),
%%     Reason = term()
addgame(GameInfo) ->
    transaction(
      fun() ->
              GameID =
                  case mnesia:read({?dbt_games, counter}) of
                      [HRec] -> HRec#hrec.val;
                      _ -> 1
                  end,
              ll_set_props(?dbt_games, counter, GameID + 1),
              set_game_props_(GameID, GameInfo),
              GameID
      end).

%% @doc Acknowledge the game.
%% @spec game_ack(GameID, Username) -> ok | {error, Reason}
%%     GameID = echessd_game:echessd_game_id(),
%%     Username = echessd_user:echessd_user(),
%%     Reason = term()
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
%% @spec game_deny(GameID, Username) -> ok | {error, Reason}
%%     GameID = echessd_game:echessd_game_id(),
%%     Username = echessd_user:echessd_user(),
%%     Reason = term()
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

%% @doc Fetch game properties.
%% @spec get_game_props(GameID) -> {ok, GameInfo} | {error, Reason}
%%     GameID = echessd_game:echessd_game_id(),
%%     GameInfo = echessd_game:echessd_game_info(),
%%     Reason = term()
get_game_props(GameID) ->
    transaction(
      fun() ->
              ll_get_props(?dbt_games, GameID)
      end).

%% @doc Set game properties.
%% @spec set_game_props(GameID, GameInfo) -> ok | {error, Reason}
%%     GameID = echessd_game:echessd_game_id(),
%%     GameInfo = echessd_game:echessd_game_info(),
%%     Reason = term()
set_game_props(GameID, GameInfo) ->
    transaction_ok(
      fun() ->
              OldInfo = ll_get_props(?dbt_games, GameID),
              NewInfo =
                  echessd_lib:proplist_replace(
                    OldInfo, GameInfo),
              set_game_props_(GameID, NewInfo)
      end).
set_game_props_(GameID, GameInfo) ->
    Users =
        [N || {users, L} <- GameInfo,
              {N, _Role} <- L],
    lists:foreach(
      fun(Username) ->
              UserInfo = ll_get_props(?dbt_users, Username),
              UserGames =
                  lists:usort(
                    [GameID | proplists:get_value(
                                games, UserInfo, [])]),
              ll_replace_props(
                ?dbt_users, Username, UserInfo,
                [{games, UserGames}])
      end, Users),
    ll_set_props(?dbt_games, GameID, GameInfo).

%% @doc Remove game from database.
%% @spec delgame(GameID) -> ok | {error, Reason}
%%     GameID = echessd_game:echessd_game_id(),
%%     Reason = term()
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
                    proplists:get_value(
                      games, UserInfo, []) -- [GameID]),
              ll_replace_props(
                ?dbt_users, Username, UserInfo,
                [{games, UserGames}])
      end, Users),
    mnesia:delete({?dbt_games, GameID}).

%% @doc Makes turn for user.
%% @spec gameply(GameID, Username, Ply) -> {ok, GameInfo} | {error, Reason}
%%     GameID = echessd_game:echessd_game_id(),
%%     Username = echessd_user:echessd_user(),
%%     Ply = echessd_game:echessd_ply(),
%%     GameInfo = echessd_game:echessd_game_info(),
%%     Reason = term()
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
                  none -> nop;
                  _ ->
                      mnesia:abort(game_ended)
              end,
              case echessd_game:who_must_turn(GameInfo) of
                  Username ->
                      TurnColor = echessd_game:turn_color(GameInfo),
                      GameType = proplists:get_value(type, GameInfo),
                      History = proplists:get_value(moves, GameInfo, []),
                      {Board, _Captures} =
                          echessd_game:from_scratch(GameType, History),
                      case echessd_game:is_valid_ply(
                             GameType, Board, TurnColor, Ply, History) of
                          {ok, _NewBoard, NewHistory, GameStatus} ->
                              {Winner, WinnerColor} =
                                  case GameStatus of
                                      checkmate -> {Username, TurnColor};
                                      _ -> {undefined, undefined}
                                  end,
                              NewGameInfo =
                                  echessd_lib:proplist_replace(
                                    GameInfo,
                                    [{moves, NewHistory},
                                     {status, GameStatus},
                                     {winner, Winner},
                                     {winner_color, WinnerColor}]),
                              ll_set_props(?dbt_games, GameID, NewGameInfo),
                              NewGameInfo;
                          {error, Reason} ->
                              mnesia:abort({wrong_move, Reason})
                      end;
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

%% @doc Make user fail the game by giving up.
%% @spec game_give_up(GameID, Username) -> ok | {error, Reason}
%%     GameID = echessd_game:echessd_game_id(),
%%     Username = echessd_user:echessd_user(),
%%     Reason = term()
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
                  none ->
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

%% @doc Makes draw request.
%% @spec game_request_draw(GameID, Username) -> ok | {error, Reason}
%%     GameID = echessd_game:echessd_game_id(),
%%     Username = echessd_user:echessd_user(),
%%     Reason = term()
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
                  none ->
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

%% @doc Denies last ply for specified game.
%% @spec gamerewind(GameID) -> ok | {error, Reason}
%%     GameID = echessd_game:echessd_game_id(),
%%     Reason = term()
gamerewind(GameID) ->
    transaction_ok(
      fun() ->
              GameInfo = ll_get_props(?dbt_games, GameID),
              History = proplists:get_value(moves, GameInfo, []),
              NewHistory = all_except_last(History),
              ll_replace_props(
                ?dbt_games, GameID, GameInfo,
                [{moves, NewHistory}, {status, none},
                 {winner, undefined},
                 {winner_color, undefined}])
      end).

%% @doc Denies all plies for specified game.
%% @spec gamereset(GameID) -> ok | {error, Reason}
%%     GameID = echessd_game:echessd_game_id(),
%%     Reason = term()
gamereset(GameID) ->
    transaction_ok(
      fun() ->
              GameInfo = ll_get_props(?dbt_games, GameID),
              ll_replace_props(
                ?dbt_games, GameID, GameInfo,
                [{moves, []}, {status, none},
                 {winner, undefined},
                 {winner_color, undefined}])
      end).

%% @doc Return all users data from database.
%% @spec dump_users() -> {ok, List} | {error, Reason}
%%     List = [{Username, UserInfo}],
%%     Username = echessd_user:echessd_user(),
%%     UserInfo = echessd_user:echessd_user_info(),
%%     Reason = term()
dump_users() ->
    transaction(
      fun() ->
              mnesia:foldl(
                fun(HRec, Acc) ->
                        [{HRec#hrec.key, HRec#hrec.val} | Acc]
                end, [], ?dbt_users)
      end).

%% @doc Return all games data from database.
%% @spec dump_games() -> {ok, List} | {error, Reason}
%%     List = [{GameID, GameInfo}],
%%     GameID = echessd_game:echessd_game_id(),
%%     GameInfo = echessd_game:echessd_game_info(),
%%     Reason = term()
dump_games() ->
    transaction(
      fun() ->
              mnesia:foldl(
                fun(HRec, Acc) ->
                        [{HRec#hrec.key, HRec#hrec.val} | Acc]
                end, [], ?dbt_games)
      end).

%% @doc Inserts user records directly to database.
%%      This low level utility function intended only for
%%      import operations.
%% @spec import_users(Users) -> ok | {error, Reason}
%%     Users = [{Username, UserInfo}],
%%     Username = echessd_user:echessd_user(),
%%     UserInfo = echessd_user:echessd_user_info(),
%%     Reason = term()
import_users(Users) ->
    transaction_ok(
      fun() ->
              lists:foreach(
                fun({Username, UserInfo}) ->
                        ll_set_props(?dbt_users, Username, UserInfo)
                end, Users)
      end).

%% @doc Inserts game records directly to database.
%%      This low level utility function intended only for
%%      import operations.
%% @spec import_games(Games) -> ok | {error, Reason}
%%     Games = [{GameID, GameInfo}],
%%     GameID = echessd_game:echessd_game_id(),
%%     GameInfo = echessd_game:echessd_game_info(),
%%     Reason = term()
import_games(Games) ->
    transaction_ok(
      fun() ->
              lists:foreach(
                fun({GameID, GameInfo}) ->
                        ll_set_props(?dbt_games, GameID, GameInfo)
                end, Games)
      end).

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

all_except_last([]) -> [];
all_except_last([_]) -> [];
all_except_last([H | Tail]) -> [H | all_except_last(Tail)].

transaction_ok(Fun) ->
    case transaction(Fun) of
        {ok, _} -> ok;
        Error -> Error
    end.

transaction(Fun) ->
    case mnesia:transaction(Fun) of
        {atomic, Result} ->
            {ok, Result};
        {aborted, Reason} ->
            {error, Reason}
    end.

ll_replace_props(DbTable, RecID, OldProps, Props2Replace) ->
    ll_set_props(
      DbTable, RecID,
      echessd_lib:proplist_replace(OldProps, Props2Replace)).

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

ll_set_props(DbTable, RecID, Props) ->
    mnesia:write(
      DbTable,
      #hrec{key = RecID,
            val = Props},
      write).

