-module(echessd_game).

-export([add/5,
         getprops/1,
         new/1,
         move/2, move/3,
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

getprops(ID) ->
    echessd_db:get_game_props(ID).

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

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

decode_coord([Col, Row]) ->
    decode_coord(Col, Row).
decode_coord(Col, Row) ->
    {8 - Row + $1, Col - $a + 1}.

