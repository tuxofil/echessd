-module(echessd_game).

-export([add/5,
         fetch/1,
         who_must_turn/1,
         turn_color/1,
         do_moves/2,
         move/3,
         getprops/1,
         transpose/1,
         new/1,
         getcell/2, setcell/3
        ]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

add(Type, Owner, OwnerColor, Opponent, OtherProps) ->
    Props =
        [{type, Type},
         {time, now()},
         {creator, Owner},
         {users,
          [{Owner, OwnerColor},
           {Opponent, hd([?black, ?white] -- [OwnerColor])}]}
         | OtherProps],
    case echessd_db:addgame(Props) of
        {ok, ID} = Ok ->
            echessd_log:info(
              "game ~9999p created: ~9999p",
              [ID, Props]),
            Ok;
        {error, Reason} = Error ->
            echessd_log:err(
              "game creation failed: ~9999p (props=~99999p)",
              [Reason, Props]),
            Error
    end.

%% @doc Fetches game from database.
%% @spec fetch(ID) -> {ok, GameInfo, {Table, TookedFigures}} | {error, Reason}
%%     ID = integer(),
%%     GameInfo = proplist(),
%%     Table = chess_table(),
%%     TookedFigures = [chess_figure()],
%%     Reason = term()
fetch(ID) ->
    case echessd_db:get_game_props(ID) of
        {ok, GameInfo} ->
            GameType = proplists:get_value(type, GameInfo),
            Moves = proplists:get_value(moves, GameInfo, []),
            {ok, GameInfo, do_moves(GameType, Moves)};
        Error -> Error
    end.

%% @doc Do all moves in game.
%% @spec do_moves(GameType, Moves) -> {Game, Tooked}
%%     GameType = game_type(),
%%     Moves = [chess_move()],
%%     Game = chess_game(),
%%     Tooked = [chess_figure()]
do_moves(GameType, Moves) ->
    lists:foldl(
      fun(Move, {Game, Tooked}) ->
              {NewGame, Took} = ll_move(Game, Move),
              {NewGame, [Took | Tooked]}
      end, {new(GameType), []}, Moves).

who_must_turn(GameInfo) ->
    Users =
        [{C, N} || {users, L} <- GameInfo,
                   {N, C} <- L,
                   lists:member(C, [?black, ?white])],
    proplists:get_value(turn_color(GameInfo), Users).

turn_color(GameInfo) ->
    case length(proplists:get_value(moves, GameInfo, [])) rem 2 of
        0 -> ?white;
        _ -> ?black
    end.

move(Game, User, Move) ->
    case echessd_db:gamemove(Game, User, Move) of
        {ok, _} ->
            echessd_log:info(
              "game ~9999p: user ~9999p moved ~9999p",
              [Game, User, Move]),
            ok;
        {error, Reason} = Error ->
            echessd_log:err(
              "game ~9999p: user ~9999p failed to move ~9999p: ~99999p",
              [Game, User, Move, Reason]),
            Error
    end.

getprops(ID) ->
    echessd_db:get_game_props(ID).

transpose(Game) ->
    list_to_tuple(
      lists:reverse(
        [list_to_tuple(
           lists:reverse(
             tuple_to_list(R))) ||
            R <- tuple_to_list(Game)])).

new(?GAME_CLASSIC) ->
    {{?brook,?bknight,?bbishop,?bqueen,?bking,?bbishop,?bknight,?brook}, %% 8
     {?bpawn,?bpawn,?bpawn,?bpawn,?bpawn,?bpawn,?bpawn,?bpawn}, %% 7
     {?empty,?empty,?empty,?empty,?empty,?empty,?empty,?empty}, %% 6
     {?empty,?empty,?empty,?empty,?empty,?empty,?empty,?empty}, %% 5
     {?empty,?empty,?empty,?empty,?empty,?empty,?empty,?empty}, %% 4
     {?empty,?empty,?empty,?empty,?empty,?empty,?empty,?empty}, %% 3
     {?wpawn,?wpawn,?wpawn,?wpawn,?wpawn,?wpawn,?wpawn,?wpawn}, %% 2
     {?wrook,?wknight,?wbishop,?wqueen,?wking,?wbishop,?wknight,?wrook}  %% 1
     %% a,b,c,d,e,f,g,h
    };
new(Other) ->
    echessd_log:err(
      "Failed to generate game of ~99999p style: not implemented",
      [Other]),
    throw({unsupported_game_style, Other}).

ll_move(Game, [A, B, C, D]) ->
    ll_move(Game, [A, B], [C, D]).
ll_move(Game, Coord1, Coord2) ->
    case {decode_coord(Coord1), decode_coord(Coord2)} of
        {{R, C1}, {R, C2}} ->
            Row = element(R, Game),
            Figure = element(C1, Row),
            Took = element(C2, Row),
            {setelement(
               R, Game,
               setelement(
                 C1,
                 setelement(
                   C2, Row, Figure
                  ), ?empty
                )), Took};
        {{R1, C1}, {R2, C2}} ->
            Row1 = element(R1, Game),
            Row2 = element(R2, Game),
            Figure = element(C1, Row1),
            Took = element(C2, Row2),
            {setelement(
               R2,
               setelement(
                 R1,
                 Game,
                 setelement(C1, Row1, ?empty)),
               setelement(C2, Row2, Figure)), Took}
    end.

getcell(Game, Coord) ->
    {R, C} = decode_coord(Coord),
    element(C, element(R, Game)).

setcell(Game, Coord, Figure) ->
    {R, C} = decode_coord(Coord),
    setelement(
      R, Game,
      setelement(C, element(R, Game), Figure)).

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

decode_coord([Col, Row]) ->
    decode_coord(Col, Row).
decode_coord(Col, Row) ->
    {8 - Row + $1, Col - $a + 1}.

