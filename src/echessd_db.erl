-module(echessd_db).

-export([init/0, wait/0,
         list_users/0,
         adduser/2,
         deluser/1,
         get_user_props/1,
         set_user_props/2,
         addgame/1,
         get_game_props/1,
         set_game_props/2,
         delgame/1,
         gamemove/3
        ]).

-include("echessd.hrl").

%% db table record
-record(hrec, {key, val}).

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

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

list_users() ->
    transaction(
      fun() ->
              mnesia:foldl(
                fun(HRec, Acc) ->
                        [HRec#hrec.key | Acc]
                end, [], ?dbt_users)
      end).

adduser(Name, Props) ->
    transaction(
      fun() ->
              case mnesia:read({?dbt_users, Name}) of
                  [_] ->
                      mnesia:abort({user_already_exists, Name});
                  _ ->
                      ll_set_props(?dbt_users, Name, Props)
              end
      end).

deluser(Name) ->
    transaction(
      fun() ->
              mnesia:delete({?dbt_users, Name})
      end).

get_user_props(Name) ->
    transaction(
      fun() ->
              ll_get_props(?dbt_users, Name)
      end).

set_user_props(Name, Props) ->
    transaction(
      fun() ->
              OldProps = ll_get_props(?dbt_users, Name),
              ll_replace_props(
                ?dbt_users, Name, OldProps, Props)
      end).

addgame(Props) ->
    transaction(
      fun() ->
              ID =
                  case mnesia:read({?dbt_games, counter}) of
                      [HRec] -> HRec#hrec.val;
                      _ -> 1
                  end,
              ll_set_props(?dbt_games, counter, ID + 1),
              set_game_props_(ID, Props),
              ID
      end).

get_game_props(ID) ->
    transaction(
      fun() ->
              ll_get_props(?dbt_games, ID)
      end).

set_game_props(ID, Props) ->
    transaction(
      fun() ->
              OldProps = ll_get_props(?dbt_games, ID),
              NewProps =
                  echessd_lib:proplist_replace(
                    OldProps, Props),
              set_game_props_(ID, NewProps)
      end).
set_game_props_(ID, Props) ->
    Users =
        [N || {users, L} <- Props,
              {N, _Role} <- L],
    lists:foreach(
      fun(User) ->
              UserProps = ll_get_props(?dbt_users, User),
              UserGames =
                  lists:usort([ID | proplists:get_value(
                                      games, UserProps, [])]),
              ll_replace_props(
                ?dbt_users, User, UserProps, [{games, UserGames}])
      end, Users),
    ll_set_props(?dbt_games, ID, Props).

delgame(ID) ->
    transaction(
      fun() ->
              case mnesia:read({?dbt_games, ID}) of
                  [HRec] ->
                      Props = HRec#hrec.val,
                      Users =
                          [N || {users, L} <- Props,
                                {N, _Role} <- L],
                      lists:foreach(
                        fun(User) ->
                                UserProps = ll_get_props(?dbt_users, User),
                                UserGames =
                                    lists:usort(
                                      proplists:get_value(
                                        games, UserProps, []) -- [ID]),
                                ll_replace_props(
                                  ?dbt_users, User, UserProps, [{games, UserGames}])
                        end, Users),
                      mnesia:delete({?dbt_games, ID});
                  _ -> ok
              end
      end).

gamemove(Game, User, Move) ->
    transaction(
      fun() ->
              GameInfo = ll_get_props(?dbt_games, Game),
              _UserInfo = ll_get_props(?dbt_users, User),
              case echessd_game:who_must_turn(GameInfo) of
                  User ->
                      TurnColor = echessd_game:turn_color(GameInfo),
                      GameType = proplists:get_value(type, GameInfo),
                      Moves = proplists:get_value(moves, GameInfo, []),
                      {Table, _Tooked} = echessd_game:do_moves(GameType, Moves),
                      case echessd_game:is_valid_move(
                             GameType, Table, TurnColor, Move) of
                          ok ->
                              ll_replace_props(
                                ?dbt_games, Game, GameInfo,
                                [{moves, Moves ++ [Move]}]);
                          {error, Reason} ->
                              mnesia:abort({wrong_move, Reason})
                      end;
                  _ ->
                      case lists:member(
                             User, [N || {users, L} <- GameInfo,
                                         {N, Role} <- L,
                                         lists:member(Role, [?white, ?black])]) of
                          true ->
                              mnesia:abort(not_your_turn);
                          _ ->
                              mnesia:abort(not_your_game)
                      end
              end
      end).

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

transaction(Fun) ->
    case mnesia:transaction(Fun) of
        {atomic, Result} ->
            {ok, Result};
        {aborted, Reason} ->
            {error, Reason}
    end.

ll_replace_props(Table, ID, OldProps, Props2Replace) ->
    NewProps = echessd_lib:proplist_replace(OldProps, Props2Replace),
    mnesia:write(
      Table,
      #hrec{key = ID,
            val = NewProps},
      write).

ll_get_props(Table, ID) ->
    case mnesia:read({Table, ID}) of
        [HRec] ->
            HRec#hrec.val;
        _ when Table == ?dbt_users ->
            mnesia:abort({no_such_user, ID});
        _ when Table == ?dbt_games ->
            mnesia:abort({no_such_game, ID});
        _ ->
            mnesia:abort({no_such_item, ID})
    end.

ll_set_props(Table, ID, Props) ->
    mnesia:write(
      Table,
      #hrec{key = ID,
            val = Props},
      write).

