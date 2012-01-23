%%%-------------------------------------------------------------------
%%% File    : echessd_game.erl
%%% Author  : Aleksey Morarash <aleksey.morarash@gmail.com>
%%% Created : 20 Jan 2012
%%% License : FreeBSD
%%% Description : upper level game processing tools
%%%
%%%-------------------------------------------------------------------

-module(echessd_game).

-export([add/5,
         is_valid_move/5,
         move/3,
         fetch/1,
         getprops/1,
         who_must_turn/1,
         turn_color/1,
         from_scratch/2,
         transpose/2
        ]).

-include("echessd.hrl").

%% @doc Game type format definition.
%% @type echessd_game_type() = string()

%% @doc Game board format definition.
%%      Commonly, this format is game-type specific
%%      and must be treated as opaque value in
%%      not 'echessd_rules_*' modules.
%% @type echessd_board() = tuple()

%% @doc Game properties format definition.
%% @type echessd_game_props() = [GameProperty],
%%     GameProperty = {type, echessd_game_type()} |
%%         {time, now()} | {creator, echessd_user:echessd_username()} |
%%         {users, [UserInfo]},
%%     UserInfo = {echessd_user:echessd_username(), echessd_color()}

%% @doc Color of player side.
%% @type echessd_color() = atom()

%% @doc String describing figure(s) move.
%%      Examples: "e2e4", "b7c6".
%% @type echessd_move() = string()

%% @doc Sequence of moves from first to last.
%% @type echessd_history() = [echessd_move()]

%% @doc Game identifier (in persistent storage).
%% @type echessd_game_id() = integer()

%% @doc Chess figure type format definition.
%% @type echessd_figure_type() = atom().

%% @doc Chess figure format definition.
%% @type echessd_figure() = {echessd_color(), echessd_figure_type()}

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Creates new game and stores it in persistent storage.
%%      Returns ID of created game in case of success.
%% @spec add(GameType, Owner, OwnerColor, Opponent, OtherProps) ->
%%                 {ok, GameID} | {error, Reason}
%%     GameType = echessd_game_type(),
%%     Owner = Opponent = echessd_user:echessd_username(),
%%     OwnerColor = echessd_color(),
%%     OtherProps = proplist(),
%%     GameID = echessd_game_id(),
%%     Reason = term()
add(GameType, Owner, OwnerColor, Opponent, OtherProps) ->
    Props =
        [{type, GameType},
         {time, now()},
         {creator, Owner},
         {users,
          [{Owner, OwnerColor},
           {Opponent, hd([?black, ?white] -- [OwnerColor])}]}
         | OtherProps],
    case echessd_db:addgame(Props) of
        {ok, GameID} = Ok ->
            echessd_log:info(
              "game ~9999p created: ~9999p",
              [GameID, Props]),
            Ok;
        {error, Reason} = Error ->
            echessd_log:err(
              "game creation failed: ~9999p (props=~99999p)",
              [Reason, Props]),
            Error
    end.

%% @doc Checks if move is valid.
%% @spec is_valid_move(GameType, Board, TurnColor, Move, History) ->
%%                 ok | {error, Reason}
%%     GameType = echessd_game_type(),
%%     Board = echessd_board(),
%%     TurnColor = echessd_color(),
%%     Move = echessd_move(),
%%     History = echessd_history(),
%%     Reason = term()
is_valid_move(?GAME_CLASSIC, Board, TurnColor, Move, History) ->
    echessd_rules_classic:is_valid_move(
      Board, TurnColor, Move, History);
is_valid_move(GameType, _, _, _, _) ->
    soft_unsupported(GameType).

%% @doc Tries to save user turn to database.
%%      Move supplied will be checked for validity.
%% @spec move(GameID, User, Move) -> ok | {error, Reason}
%%     GameID = echessd_game_id(),
%%     User = echessd_user:echessd_username(),
%%     Move = echessd_move(),
%%     Reason = term()
move(GameID, User, Move) ->
    case echessd_db:gamemove(GameID, User, Move) of
        ok ->
            echessd_log:info(
              "game ~9999p: user ~9999p moved ~9999p",
              [GameID, User, Move]),
            ok;
        {error, Reason} = Error ->
            echessd_log:err(
              "game ~9999p: user ~9999p failed to move ~9999p: ~99999p",
              [GameID, User, Move, Reason]),
            Error
    end.

%% @doc Fetches game from database.
%% @spec fetch(GameID) -> {ok, GameInfo, {Board, Captures}} | {error, Reason}
%%     GameID = echessd_game_id(),
%%     GameInfo = echessd_game_props(),
%%     Board = echessd_board(),
%%     Captures = [echessd_figure()],
%%     Reason = term()
fetch(GameID) ->
    case getprops(GameID) of
        {ok, GameInfo} ->
            GameType = proplists:get_value(type, GameInfo),
            History = proplists:get_value(moves, GameInfo, []),
            {ok, GameInfo, from_scratch(GameType, History)};
        Error -> Error
    end.

%% @doc Fetches game properties from database.
%% @spec getprops(GameID) -> {ok, GameInfo} | {error, Reason}
%%     GameID = echessd_game_id(),
%%     GameInfo = echessd_game_props(),
%%     Reason = term()
getprops(GameID) ->
    echessd_db:get_game_props(GameID).

%% @doc Tells username who must move now.
%% @spec who_must_turn(GameInfo) -> Username
%%     GameInfo = echessd_game_props(),
%%     Username = echessd_user:echessd_username()
who_must_turn(GameInfo) ->
    Users =
        [{C, N} || {users, L} <- GameInfo,
                   {N, C} <- L,
                   lists:member(C, [?black, ?white])],
    proplists:get_value(turn_color(GameInfo), Users).

%% @doc Tells color of the side which must move.
%% @spec turn_color(GameInfo) -> Color
%%     GameInfo = echessd_game_props(),
%%     Color = echessd_color()
turn_color(GameInfo) ->
    case length(proplists:get_value(moves, GameInfo, [])) rem 2 of
        0 -> ?white;
        _ -> ?black
    end.

%% @doc Creates new board and makes all moves from history supplied.
%%      Returns result chess board and all figures captured.
%% @spec do_moves(GameType, History) -> {Board, Captures}
%%     GameType = echessd_game_type(),
%%     History = echessd_history(),
%%     Board = echessd_board(),
%%     Captures = [echessd_figure()]
from_scratch(GameType, History) ->
    lists:foldl(
      fun(Move, {Board, Captures}) ->
              {NewGame, Capture} =
                  move_figure(GameType, Board, Move),
              {NewGame, [Capture | Captures]}
      end, {new(GameType), []}, History).

%% @doc Turns internal board representation at 180 degrees.
%% @spec transpose(GameType, Board) -> NewBoard
%%     GameType = echessd_game_type(),
%%     Board = NewBoard = echessd_board()
transpose(?GAME_CLASSIC, Board) ->
    echessd_rules_classic:transpose(Board);
transpose(GameType, _Board) ->
    unsupported(GameType).

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc Creates chess board with all figures at start point.
%% @spec new(GameType) -> Board
%%     GameType = echessd_game_type(),
%%     Board = echessd_board()
new(?GAME_CLASSIC) ->
    echessd_rules_classic:new();
new(GameType) ->
    echessd_log:err(
      "Failed to generate game of ~99999p style: not implemented",
      [GameType]),
    unsupported(GameType).

%% @doc Make figure move by rules of specified game.
%% @spec move_figure(GameType, Board, Move) -> {NewBoard, Capture}
%%     GameType = echessd_game_type(),
%%     Board = NewBoard = echessd_board(),
%%     Capture = echessd_figure()
move_figure(?GAME_CLASSIC, Board, Move) ->
    echessd_rules_classic:move_figure(Board, Move);
move_figure(GameType, _Board, _Move) ->
    unsupported(GameType).

unsupported(GameType) -> throw(unsupported_reason(GameType)).
soft_unsupported(GameType) -> {error, unsupported_reason(GameType)}.
unsupported_reason(GameType) -> {unsupported_game_type, GameType}.

