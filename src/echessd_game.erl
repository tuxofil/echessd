%%%-------------------------------------------------------------------
%%% File    : echessd_game.erl
%%% Author  : Aleksey Morarash <aleksey.morarash@gmail.com>
%%% Created : 21 Jan 2012
%%% License : FreeBSD
%%% Description : upper level game processing tools
%%%
%%%-------------------------------------------------------------------

-module(echessd_game).

-export([add/5,
         is_valid_ply/5,
         ply/3,
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

%% @doc String describing half-move (ply).
%%      Examples: "e2e4", "b7c6".
%% @type echessd_ply() = string()

%% @doc Sequence of plies from first to last.
%% @type echessd_history() = [echessd_ply()]

%% @doc Game identifier (in persistent storage).
%% @type echessd_game_id() = integer()

%% @doc Chessman type format definition.
%% @type echessd_chessman_type() = atom().

%% @doc Chessman format definition.
%% @type echessd_chessman() = {echessd_color(), echessd_chessman_type()}

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

%% @doc Checks if half-move is valid.
%% @spec is_valid_ply(GameType, Board, TurnColor, Ply, History) ->
%%                 ok | {error, Reason}
%%     GameType = echessd_game_type(),
%%     Board = echessd_board(),
%%     TurnColor = echessd_color(),
%%     Ply = echessd_ply(),
%%     History = echessd_history(),
%%     Reason = term()
is_valid_ply(?GAME_CLASSIC, Board, TurnColor, Ply, History) ->
    echessd_rules_classic:is_valid_ply(
      Board, TurnColor, Ply, History);
is_valid_ply(GameType, _, _, _, _) ->
    soft_unsupported(GameType).

%% @doc Tries to save user turn to database.
%%      Turn supplied will be checked for validity.
%% @spec ply(GameID, User, Ply) -> ok | {error, Reason}
%%     GameID = echessd_game_id(),
%%     User = echessd_user:echessd_username(),
%%     Ply = echessd_ply(),
%%     Reason = term()
ply(GameID, User, Ply) ->
    case echessd_db:gameply(GameID, User, Ply) of
        ok ->
            echessd_log:info(
              "game ~9999p: user ~9999p moved ~9999p",
              [GameID, User, Ply]),
            ok;
        {error, Reason} = Error ->
            echessd_log:err(
              "game ~9999p: user ~9999p failed to move ~9999p: ~99999p",
              [GameID, User, Ply, Reason]),
            Error
    end.

%% @doc Fetches game from database.
%% @spec fetch(GameID) -> {ok, GameInfo, {Board, Captures}} | {error, Reason}
%%     GameID = echessd_game_id(),
%%     GameInfo = echessd_game_props(),
%%     Board = echessd_board(),
%%     Captures = [echessd_chessman()],
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

%% @doc Tells username who must turn now.
%% @spec who_must_turn(GameInfo) -> Username
%%     GameInfo = echessd_game_props(),
%%     Username = echessd_user:echessd_username()
who_must_turn(GameInfo) ->
    Users =
        [{C, N} || {users, L} <- GameInfo,
                   {N, C} <- L,
                   lists:member(C, [?black, ?white])],
    proplists:get_value(turn_color(GameInfo), Users).

%% @doc Tells color of the side which must turn.
%% @spec turn_color(GameInfo) -> Color
%%     GameInfo = echessd_game_props(),
%%     Color = echessd_color()
turn_color(GameInfo) ->
    case length(proplists:get_value(moves, GameInfo, [])) rem 2 of
        0 -> ?white;
        _ -> ?black
    end.

%% @doc Creates new board and makes all moves from history supplied.
%%      Returns result chess board and all chessmans captured.
%% @spec from_scratch(GameType, History) -> {Board, Captures}
%%     GameType = echessd_game_type(),
%%     History = echessd_history(),
%%     Board = echessd_board(),
%%     Captures = [echessd_chessman()]
from_scratch(GameType, History) ->
    lists:foldl(
      fun(Ply, {Board, Captures}) ->
              {NewGame, Capture} =
                  move_chessman(GameType, Board, Ply),
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

%% @doc Creates chess board with all chessmans at start point.
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

%% @doc Make chessman move by rules of specified game.
%% @spec move_chessman(GameType, Board, Ply) -> {NewBoard, Capture}
%%     GameType = echessd_game_type(),
%%     Board = NewBoard = echessd_board(),
%%     Capture = echessd_chessman()
move_chessman(?GAME_CLASSIC, Board, Ply) ->
    echessd_rules_classic:move_chessman(Board, Ply);
move_chessman(GameType, _Board, _Ply) ->
    unsupported(GameType).

unsupported(GameType) -> throw(unsupported_reason(GameType)).
soft_unsupported(GameType) -> {error, unsupported_reason(GameType)}.
unsupported_reason(GameType) -> {unsupported_game_type, GameType}.

