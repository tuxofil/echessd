-module(echessd_game).

-export([new/0,
         move/2, move/3,
         getcell/2, setcell/3
        ]).

-compile(export_all).

-include("echessd.hrl").

new() ->
    {{?b_rook,?b_knight,?b_bishop,?b_queen,?b_king,?b_bishop,?b_knight,?b_rook}, %% 8
     {?b_pawn,?b_pawn,?b_pawn,?b_pawn,?b_pawn,?b_pawn,?b_pawn,?b_pawn}, %% 7
     {?empty,?empty,?empty,?empty,?empty,?empty,?empty,?empty}, %% 6
     {?empty,?empty,?empty,?empty,?empty,?empty,?empty,?empty}, %% 5
     {?empty,?empty,?empty,?empty,?empty,?empty,?empty,?empty}, %% 4
     {?empty,?empty,?empty,?empty,?empty,?empty,?empty,?empty}, %% 3
     {?w_pawn,?w_pawn,?w_pawn,?w_pawn,?w_pawn,?w_pawn,?w_pawn,?w_pawn}, %% 2
     {?w_rook,?w_knight,?w_bishop,?w_queen,?w_king,?w_bishop,?w_knight,?w_rook}  %% 1
     %% a,b,c,d,e,f,g,h
    }.

move(Game, [A, B, C, D]) ->
    move(Game, [A, B], [C, D]).
move(Game, Coord1, Coord2) ->
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

decode_coord([Col, Row]) ->
    decode_coord(Col, Row).
decode_coord(Col, Row) ->
    {8 - Row + $1, Col - $a + 1}.

