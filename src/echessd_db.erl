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
                               ?ui_show_in_list, HRec#hrec.val) of
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
                      mnesia:abort({?e_user_already_exists, Username});
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
              [{?ui_login, Username} |
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
              set_game_props_(GameID = generate_game_id(), GameInfo),
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
              case proplists:get_value(?gi_creator, GameInfo) of
                  Username ->
                      mnesia:abort(?e_not_your_game);
                  _ -> nop
              end,
              List =
                  [z || {?gi_users, [_ | _] = L} <- GameInfo,
                        {N, C} <- L, N == Username,
                        lists:member(C, [?white, ?black])],
              if length(List) > 0 -> nop;
                 true ->
                      mnesia:abort(?e_not_your_game)
              end,
              case proplists:get_value(?gi_acknowledged, GameInfo) of
                  true -> ok;
                  _ ->
                      ll_replace_props(
                        ?dbt_games, GameID, GameInfo,
                        [{?gi_acknowledged, true}])
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
                  [z || {?gi_users, [_ | _] = L} <- GameInfo,
                        {N, C} <- L, N == Username,
                        lists:member(C, [?white, ?black])],
              if length(List) > 0 -> nop;
                 true ->
                      mnesia:abort(?e_not_your_game)
              end,
              case proplists:get_value(?gi_acknowledged, GameInfo) of
                  true ->
                      mnesia:abort(?e_unable_to_deny_confirmed_game);
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
    Users = [N || {?gi_users, L} <- GameInfo, {N, _Role} <- L],
    lists:foreach(
      fun(Username) ->
              UserInfo = ll_get_props(?dbt_users, Username),
              UserGames =
                  lists:usort(
                    [GameID | echessd_user:get_value(?ui_games, UserInfo)]),
              ll_replace_props(
                ?dbt_users, Username, UserInfo,
                [{?ui_games, UserGames}])
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
    Users = [N || {?gi_users, L} <- GameInfo, {N, _Role} <- L],
    lists:foreach(
      fun(Username) ->
              UserInfo = ll_get_props(?dbt_users, Username),
              UserGames =
                  lists:usort(
                    echessd_user:get_value(
                      ?ui_games, UserInfo) -- [GameID]),
              ll_replace_props(
                ?dbt_users, Username, UserInfo,
                [{?ui_games, UserGames}])
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
              {GameInfo, CurrentColor, _OpponentName, _OpponentColor} =
                  prepare_to_move(GameID, Username),
              GameType = proplists:get_value(?gi_type, GameInfo),
              History = proplists:get_value(?gi_history, GameInfo, []),
              GameInfoUpdates =
                  case echessd_game:move(GameType, History, Ply) of
                      {ok, _, NewHistory, ?gs_checkmate = NewStatus} ->
                          [{?gi_history, NewHistory},
                           {?gi_status, NewStatus},
                           {?gi_winner, Username},
                           {?gi_winner_color, CurrentColor}];
                      {ok, _, NewHistory, NewStatus} ->
                          [{?gi_history, NewHistory},
                           {?gi_status, NewStatus},
                           {?gi_winner, undefined},
                           {?gi_winner_color, undefined}]
                  end,
              NewGameInfo = proplist_update(GameInfo, GameInfoUpdates),
              ll_set_props(?dbt_games, GameID, NewGameInfo),
              NewGameInfo
      end).

%% @doc Make the user to fail the game by giving up.
-spec game_give_up(GameID :: echessd_game:id(),
                   Username :: echessd_user:name()) ->
                          ok | {error, Reason :: any()}.
game_give_up(GameID, Username) ->
    transaction_ok(
      fun() ->
              {GameInfo, _CurrentColor, OpponentName, OpponentColor} =
                  prepare_to_move(GameID, Username),
              ll_replace_props(
                ?dbt_games, GameID, GameInfo,
                [{?gi_status, ?gs_give_up},
                 {?gi_winner, OpponentName},
                 {?gi_winner_color, OpponentColor}])
      end).

%% @doc Make a draw request.
-spec game_request_draw(GameID :: echessd_game:id(),
                        Username :: echessd_user:name()) ->
                               ok | {error, Reason :: any()}.
game_request_draw(GameID, Username) ->
    transaction_ok(
      fun() ->
              GameInfo = ll_get_props(?dbt_games, GameID),
              case proplists:is_defined(?gi_acknowledged, GameInfo) of
                  true -> nop;
                  _ ->
                      mnesia:abort(?e_game_not_acknowledged)
              end,
              %% check if such user exists
              _UserInfo = ll_get_props(?dbt_users, Username),
              Players =
                  [Player || {?gi_users, [_ | _] = Watchers} <- GameInfo,
                        {_Name, Color} = Player <- Watchers,
                        lists:member(Color, [?white, ?black])],
              case proplists:is_defined(Username, Players) of
                  true -> nop;
                  false ->
                      mnesia:abort(?e_not_your_game)
              end,
              {_CurrentPlayerName, CurrentColor} =
                  echessd_game:current_player(GameInfo),
              [Opponent | _] = [N || {N, C} <- Players, C /= CurrentColor],
              OldDrawRequestUser =
                  proplists:get_value(?gi_draw_request_from, GameInfo),
              case proplists:get_value(?gi_status, GameInfo) of
                  ?gs_alive when Username == Opponent ->
                      %% test game: just end the game
                      ll_replace_props(
                        ?dbt_games, GameID, GameInfo,
                        [{?gi_status, ?gs_draw_agreement},
                         {?gi_winner, undefined},
                         {?gi_winner_color, undefined}]);
                  ?gs_alive when OldDrawRequestUser == Opponent ->
                      %% confirm draw
                      ll_replace_props(
                        ?dbt_games, GameID, GameInfo,
                        [{?gi_status, ?gs_draw_agreement},
                         {?gi_winner, undefined},
                         {?gi_winner_color, undefined}]);
                  ?gs_alive ->
                      %% make draw request
                      ll_replace_props(
                        ?dbt_games, GameID, GameInfo,
                        [{?gi_draw_request_from, Username},
                         {?gi_winner, undefined},
                         {?gi_winner_color, undefined}]);
                  _ ->
                      mnesia:abort(?e_game_ended)
              end
      end).

%% @doc Cancel the last ply for the game.
-spec gamerewind(GameID :: echessd_game:id()) ->
                        ok | {error, Reason :: any()}.
gamerewind(GameID) ->
    transaction_ok(
      fun() ->
              GameInfo = ll_get_props(?dbt_games, GameID),
              History = proplists:get_value(?gi_history, GameInfo, []),
              NewHistory = all_except_last(History),
              ll_replace_props(
                ?dbt_games, GameID, GameInfo,
                [{?gi_history, NewHistory}, {?gi_status, ?gs_alive},
                 {?gi_winner, undefined},
                 {?gi_winner_color, undefined}])
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
                [{?gi_history, []}, {?gi_status, ?gs_alive},
                 {?gi_winner, undefined},
                 {?gi_winner_color, undefined}])
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
-spec prepare_to_move(GameID :: echessd_game:id(),
                      Username :: echessd_user:name()) ->
                             {GameInfo :: echessd_game:info(),
                              CurrentColor :: echessd_game:color(),
                              OpponentName :: echessd_user:name(),
                              OpponentColor :: echessd_game:color()}.
prepare_to_move(GameID, Username) ->
    GameInfo = ll_get_props(?dbt_games, GameID),
    case proplists:get_value(?gi_acknowledged, GameInfo) of
        true ->
            ok;
        _ ->
            mnesia:abort(?e_game_not_acknowledged)
    end,
    case proplists:get_value(?gi_status, GameInfo) of
        ?gs_alive ->
            ok;
        _ ->
            mnesia:abort(?e_game_ended)
    end,
    Players =
        [Player || {?gi_users, [_ | _] = Watchers} <- GameInfo,
                   {_Name, Color} = Player <- Watchers,
                   lists:member(Color, [?white, ?black])],
    case proplists:is_defined(Username, Players) of
        true ->
            ok;
        false ->
            mnesia:abort(?e_not_your_game)
    end,
    case echessd_game:current_player(GameInfo) of
        {Username, CurrentColor} ->
            {GameInfo, CurrentColor,
             hd([Name || {Name, Color} <- Players, Color /= CurrentColor]),
             hd([?white, ?black] -- [CurrentColor])};
        {_Another, _CurrentColor} ->
            mnesia:abort(?e_not_your_turn)
    end.

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
            mnesia:abort({?e_no_such_user, RecID});
        _ when DbTable == ?dbt_games ->
            mnesia:abort({?e_no_such_game, RecID});
        _ ->
            mnesia:abort({?e_no_such_item, RecID})
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
