%%% @doc
%%% Classic chess rules implementation.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 22 Jan 2012
%%% @copyright 2012, Aleksey Morarash

-module(echessd_rules_classic).

-export(
   [new/0,
    move/2,
    hint/1,
    move_unsafe/2,
    can_move/1,
    transpose/1
   ]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% Type definitions
%% ----------------------------------------------------------------------

-type entry() :: echessd_game:chessman() | ?empty.

-type board() ::
        {{entry(),entry(),entry(),entry(),entry(),entry(),entry(),entry()},
         {entry(),entry(),entry(),entry(),entry(),entry(),entry(),entry()},
         {entry(),entry(),entry(),entry(),entry(),entry(),entry(),entry()},
         {entry(),entry(),entry(),entry(),entry(),entry(),entry(),entry()},
         {entry(),entry(),entry(),entry(),entry(),entry(),entry(),entry()},
         {entry(),entry(),entry(),entry(),entry(),entry(),entry(),entry()},
         {entry(),entry(),entry(),entry(),entry(),entry(),entry(),entry()},
         {entry(),entry(),entry(),entry(),entry(),entry(),entry(),entry()}}.

-type coord() ::
        {Column :: 1..8, Row :: 1..8}.

-type step() ::
        {ColumnDelta :: -2..2, RowDelta :: -2..2}.

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Create a chess board with the all chessmans at their start point.
-spec new() -> board().
new() ->
    {
     %%  a       b        c       d       e       f        g       h
     {?brook,?bknight,?bbishop,?bqueen,?bking,?bbishop,?bknight,?brook}, % 8
     {?bpawn,?bpawn,  ?bpawn,  ?bpawn, ?bpawn,?bpawn,  ?bpawn,  ?bpawn}, % 7
     {?empty,?empty,  ?empty,  ?empty, ?empty,?empty,  ?empty,  ?empty}, % 6
     {?empty,?empty,  ?empty,  ?empty, ?empty,?empty,  ?empty,  ?empty}, % 5
     {?empty,?empty,  ?empty,  ?empty, ?empty,?empty,  ?empty,  ?empty}, % 4
     {?empty,?empty,  ?empty,  ?empty, ?empty,?empty,  ?empty,  ?empty}, % 3
     {?wpawn,?wpawn,  ?wpawn,  ?wpawn, ?wpawn,?wpawn,  ?wpawn,  ?wpawn}, % 2
     {?wrook,?wknight,?wbishop,?wqueen,?wking,?wbishop,?wknight,?wrook}  % 1
     %%  a       b        c       d       e       f        g       h
    }.

%% @doc Do the next move.
-spec move(History :: echessd_game:history(), Ply :: echessd_game:ply()) ->
                  {ok, NewBoard :: board(),
                   NewHistory :: echessd_game:history(),
                   GameStatus :: echessd_game:status()}.
move(History, {PlyCoords, PlyInfo} = Ply) ->
    {Board, Color} = restore_board_and_next_color(History),
    {SrcIndex, DstIndex, PlyCoordsTail} = ply_dec(Ply),
    {Color, _ChessmanType} = Chessman = getcell(Board, SrcIndex),
    case getcell(Board, DstIndex) of
        {Color, _} ->
            throw(friendly_fire);
        {_, ?king} ->
            throw(cannot_take_king);
        _ -> ok
    end,
    true = lists:member(
             DstIndex, hint_for_cell_unsafe(Board, SrcIndex, History)),
    {ok, NewBoard, Capture} =
        move_safe(Board, SrcIndex, DstIndex, PlyCoordsTail, History),
    NewHistory = History ++ [Ply],
    Status = status(NewBoard, not_color(Color), NewHistory),
    Notation =
        notation(Chessman, Ply, Capture, NewBoard, Status, NewHistory),
    FinalPly =
        {PlyCoords,
         [{notation, Notation} | lists:keydelete(notation, 1, PlyInfo)]},
    {ok, NewBoard, History ++ [FinalPly], Status}.

%% @doc Return a list of all available moves.
%% The function used only for next move highlighting
%% in the user interface.
-spec hint(History :: echessd_game:history()) ->
                  Plies :: [echessd_game:ply_coords()].
hint(History) ->
    {Board, Color} = restore_board_and_next_color(History),
    lists:flatmap(
      fun(I1) ->
              case getcell(Board, I1) of
                  {Color, _ChessmanType} ->
                      lists:flatmap(
                        fun(I2) ->
                                case is_valid(Board, I1, I2, History) of
                                    true ->
                                        [ind_enc(I1) ++ ind_enc(I2)];
                                    false ->
                                        []
                                end
                        end, hint_for_cell_unsafe(Board, I1, History));
                  _ -> []
              end
      end, all_cells()).

%% @doc Make the chessman move according to the chessman type.
%% No additional checks will be performed (e.g is king checked).
-spec move_unsafe(Board :: board(),
                  Ply :: echessd_game:ply() |
                         echessd_game:ply_coords()) ->
                         {NewBoard :: board(), Capture :: entry()}.
move_unsafe(Board, Ply) ->
    {I1, I2, Tail} = ply_dec(Ply),
    move_unsafe(Board, I1, I2, Tail).

%% @doc Check if valid turn is present for the color.
-spec can_move(History :: echessd_game:history()) -> boolean().
can_move(History) ->
    {Board, Color} = restore_board_and_next_color(History),
    can_move(Board, Color, History).

%% @private
%% @doc Helper for the can_move/1 fun.
-spec can_move(Board :: board(), Color :: echessd_game:color(),
               History :: echessd_game:history()) -> boolean().
can_move(Board, Color, History) ->
    lists:any(
      fun(I1) ->
              case getcell(Board, I1) of
                  {Color, _ChessmanType} ->
                      lists:any(
                        fun(I2) ->
                                is_valid(Board, I1, I2, History)
                        end, hint_for_cell_unsafe(Board, I1, History));
                  _ -> false
              end
      end, all_cells()).

%% @doc Turn the board at 180 degrees.
-spec transpose(Board :: board()) -> TransposedBoard :: board().
transpose(Board) ->
    list_to_tuple(
      lists:reverse(
        [list_to_tuple(
           lists:reverse(
             tuple_to_list(R))) || R <- tuple_to_list(Board)])).

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc
-spec move_chessman(Board :: board(),
                    SrcIndex :: coord(), DstIndex :: coord(),
                    SrcEntry :: entry(), DstEntry :: entry()) ->
                           {NewBoard :: board(), Capture :: entry()}.
move_chessman(Board, SrcIndex, DstIndex, SrcEntry, DstEntry) ->
    {setcell(setcell(Board, SrcIndex, ?empty), DstIndex, SrcEntry),
     DstEntry}.

%% @doc Return game status.
-spec status(Board :: board(), Color :: echessd_game:color(),
             History :: echessd_game:history()) ->
                    GameStatus :: echessd_game:status().
status(Board, Color, History) ->
    case can_move(Board, Color, History) of
        true ->
            none;
        false ->
            KingIndex = whereis_the_king(History, Color),
            case is_cell_attacked(Board, KingIndex, not_color(Color)) of
                false ->
                    {draw, stalemate};
                _ ->
                    checkmate
            end
    end.

%% @doc
-spec restore_board_and_next_color(History :: echessd_game:history()) ->
                                          {Board :: board(),
                                           Color :: echessd_game:color()}.
restore_board_and_next_color(History) ->
    {lists:foldl(
       fun(Ply, Acc) ->
               element(1, move_unsafe(Acc, Ply))
       end, new(), History),
     next_color(History)}.

%% @doc Helper for the move_unsafe/2 fun.
-spec move_unsafe(Board :: board(), SrcIndex :: coord(),
                  DstIndex :: coord(), Tail :: string()) ->
                         {NewBoard :: board(), Capture :: entry()}.
move_unsafe(Board, I1, I2, Tail) ->
    {MyColor, _SrcChessmanType} = F1 = getcell(Board, I1),
    F2 = getcell(Board, I2),
    case is_en_passant_simple(Board, I1, I2, F1, F2) of
        {ok, EnemyPawnIndex, EnemyPawn} ->
            Board2 = setcell(Board, EnemyPawnIndex, ?empty),
            {Board3, ?empty} = move_chessman(Board2, I1, I2, F1, F2),
            {Board3, EnemyPawn};
        _ ->
            case is_promotion(I2, F1, Tail) of
                {ok, DstChessmanType} ->
                    move_chessman(
                      Board, I1, I2, {MyColor, DstChessmanType}, F2);
                _ ->
                    case is_castling(I1, I2, F1, F2) of
                        {ok, RookPlyCoords} ->
                            {B2, _} = move_chessman(Board, I1, I2, F1, F2),
                            RookPly = {RookPlyCoords, []},
                            move_unsafe(B2, RookPly);
                        false ->
                            move_chessman(Board, I1, I2, F1, F2)
                    end
            end
    end.

%% @doc
-spec is_valid(Board :: board(), SrcIndex :: coord(),
               DstIndex :: coord(), History :: echessd_game:history()) ->
                      boolean().
is_valid(Board, SrcIndex, DstIndex, History) ->
    try
        {ok, _NewBoard, _Capture} =
            move_safe(Board, SrcIndex, DstIndex, [], History),
        true
    catch _:_ ->
            false
    end.

%% @doc
-spec all_cells() -> [coord()].
all_cells() ->
    [{C, R} || C <- lists:seq(1, 8), R <- lists:seq(1, 8)].

%% @doc
-spec next_color(History :: echessd_game:history()) ->
                        echessd_game:color().
next_color(History) when length(History) rem 2 == 0 ->
    ?white;
next_color(_) ->
    ?black.

-define(null, '*null').

%% @doc
-spec move_safe(Board :: board(), SrcIndex :: coord(), DstIndex :: coord(),
                Tail :: string(), History :: echessd_game:history()) ->
                       {ok, NewBoard :: board(), Capture :: entry()}.
move_safe(Board, I1, I2, Tail, History) ->
    {Color, ChessmanType} = getcell(Board, I1),
    {NewBoard, Capture} = move_unsafe(Board, I1, I2, Tail),
    KingIndex =
        if ChessmanType == ?king ->
                I2;
           true ->
                whereis_the_king(History, Color)
        end,
    false = is_cell_attacked(NewBoard, KingIndex, not_color(Color)),
    {ok, NewBoard, Capture}.

%% @doc Return a list of possible moves from the cell.
-spec hint_for_cell_unsafe(Board :: board(), CellCoord :: coord(),
                           History :: echessd_game:history()) ->
                                  [coord()].
hint_for_cell_unsafe(Board, CellCoord, History) ->
    case getcell(Board, CellCoord) of
        {Color, ChessmanType} ->
            lists:usort(
              lists:flatten(
                possible_(Board, CellCoord, Color, ChessmanType, History)))
                -- [?null];
        ?empty ->
            []
    end.

%% @doc
-spec possible_(Board :: board(), Index :: coord(),
                Color :: echessd_game:color(),
                ChessmanType :: echessd_game:chessman_type(),
                History :: echessd_game:history()) ->
                       [coord()].
possible_(B, I, C, ?pawn, History) ->
    {StartRow, Direction} =
        if C == ?white -> {2, 1};
           true -> {7, -1}
        end,
    F1 = ind_inc(I, {0, Direction}),
    F2 = ind_inc(I, {0, Direction * 2}),
    FL = ind_inc(I, {-1, Direction}),
    FR = ind_inc(I, {1, Direction}),
    EnPassL = ind_inc(I, {-1, 0}),
    EnPassR = ind_inc(I, {1, 0}),
    OppPawn = {not_color(C), ?pawn},
    case getcell(B, F1) of
        ?empty ->
            [F1] ++
                case ind_row(I) of
                    StartRow ->
                        case getcell(B, F2) of
                            ?empty -> [F2];
                            _ -> []
                        end;
                    _ -> []
                end;
        _ -> []
    end ++
        case getcell(B, FL) of
            {C, _} -> [];
            {_, _} -> [FL];
            _ -> []
        end ++
        case getcell(B, FR) of
            {C, _} -> [];
            {_, _} -> [FR];
            _ -> []
        end ++
        case getcell(B, EnPassL) of
            OppPawn ->
                case is_en_passant(History, EnPassL) of
                    true ->
                        [ind_inc(I, {-1, Direction})];
                    _ -> []
                end;
            _ -> []
        end ++
        case getcell(B, EnPassR) of
            OppPawn ->
                case is_en_passant(History, EnPassR) of
                    true ->
                        [ind_inc(I, {1, Direction})];
                    _ -> []
                end;
            _ -> []
        end;
possible_(B, I, C, ?rook, _History) ->
    [free_cells_until_enemy(B, C, I, {0, 1}),
     free_cells_until_enemy(B, C, I, {0, -1}),
     free_cells_until_enemy(B, C, I, {1, 0}),
     free_cells_until_enemy(B, C, I, {-1, 0})];
possible_(B, I, C, ?bishop, _History) ->
    [free_cells_until_enemy(B, C, I, {-1, -1}),
     free_cells_until_enemy(B, C, I, {1, -1}),
     free_cells_until_enemy(B, C, I, {-1, 1}),
     free_cells_until_enemy(B, C, I, {1, 1})];
possible_(B, I, C, ?queen, History) ->
    [possible_(B, I, C, ?bishop, History),
     possible_(B, I, C, ?rook, History)];
possible_(B, I, C, ?knight, _History) ->
    [is_empty_or_enemy(B, C, ind_inc(I, Step)) ||
        Step <- knight_steps()];
possible_(B, I, C, ?king, History) ->
    [is_empty_or_enemy(B, C, ind_inc(I, {-1, -1})),
     is_empty_or_enemy(B, C, ind_inc(I, {-1, 0})),
     is_empty_or_enemy(B, C, ind_inc(I, {-1, 1})),
     is_empty_or_enemy(B, C, ind_inc(I, {0, -1})),
     is_empty_or_enemy(B, C, ind_inc(I, {0, 1})),
     is_empty_or_enemy(B, C, ind_inc(I, {1, -1})),
     is_empty_or_enemy(B, C, ind_inc(I, {1, 0})),
     is_empty_or_enemy(B, C, ind_inc(I, {1, 1}))] ++
        available_castlings(B, C, History).

%% ----------------------------------------------------------------------
%% castlings

%% @doc
-spec available_castlings(Board :: board(),
                          Color :: echessd_game:color(),
                          History :: echessd_game:history()) ->
                                 [coord()].
available_castlings(Board, Color, History) ->
    KingStartCoord = king_start_coord(Color),
    case is_have_been_moved(History, KingStartCoord) orelse
        is_cell_attacked(Board, KingStartCoord, not_color(Color)) of
        true ->
            [];
        _ ->
            case long_castling(Board, Color, History) of
                ?null ->
                    [];
                LongCastlingCoord ->
                    [LongCastlingCoord]
            end ++
                case short_castling(Board, Color, History) of
                    ?null ->
                        [];
                    ShortCastlingCoord ->
                        [ShortCastlingCoord]
                end
    end.

%% @doc
-spec long_castling(Board :: board(),
                    Color :: echessd_game:color(),
                    History :: echessd_game:history()) ->
                           coord() | ?null.
long_castling(Board, Color, History) ->
    case is_have_been_moved(History, long_rook_coord(Color)) of
        true ->
            ?null;
        _ ->
            KingStartCoord = king_start_coord(Color),
            KingDstCoord = ind_inc(KingStartCoord, {-1, 0}),
            case is_cell_attacked(Board, KingDstCoord, not_color(Color)) of
                false ->
                    ind_inc(KingStartCoord, {-2, 0});
                true ->
                    ?null
            end
    end.

%% @doc
-spec short_castling(Board :: board(),
                     Color :: echessd_game:color(),
                     History :: echessd_game:history()) ->
                            coord() | ?null.
short_castling(Board, Color, History) ->
    case is_have_been_moved(History, short_rook_coord(Color)) of
        true ->
            ?null;
        _ ->
            KingStartCoord = king_start_coord(Color),
            KingDstCoord = ind_inc(KingStartCoord, {1, 0}),
            case is_cell_attacked(Board, KingDstCoord, not_color(Color)) of
                false ->
                    ind_inc(KingStartCoord, {2, 0});
                true ->
                    ?null
            end
    end.

%% @doc
-spec king_start_coord(Color :: echessd_game:color()) -> coord().
king_start_coord(?white) ->
    ind_dec("e1");
king_start_coord(?black) ->
    ind_dec("e8").

%% @doc
-spec long_rook_coord(Color :: echessd_game:color()) -> coord().
long_rook_coord(?white) ->
    ind_dec("a1");
long_rook_coord(?black) ->
    ind_dec("a8").

%% @doc
-spec short_rook_coord(Color :: echessd_game:color()) -> coord().
short_rook_coord(?white) ->
    ind_dec("h1");
short_rook_coord(?black) ->
    ind_dec("h8").

%% @doc
-spec is_have_been_moved(History :: echessd_game:history(),
                         Coord :: coord()) -> boolean().
is_have_been_moved(History, Coord) ->
    HalfPly = ind_enc(Coord),
    lists:any(
      fun({PlyCoords, _PlyInfo}) ->
              lists:prefix(HalfPly, PlyCoords)
      end, History).

%% @doc
-spec is_castling(SrcIndex :: coord(), DstIndex :: coord(),
                  SrcChessman :: echessd_game:chessman(),
                  DstChessman :: entry()) ->
                         {ok, RookPlyCoords :: echessd_game:ply_coords()} |
                         false.
is_castling({IC1, IR}, {IC2, IR}, {C, ?king}, ?empty)
  when abs(IC2 - IC1) == 2,
       ((C == ?black andalso IR == 8 andalso IC1 == 5)
        orelse (C == ?white andalso IR == 1 andalso IC1 == 5)) ->
    case {C, IC2 - IC1} of
        {?black, -2} ->
            {ok, "a8d8"};
        {?black, 2} ->
            {ok, "h8f8"};
        {?white, -2} ->
            {ok, "a1d1"};
        {?white, 2} ->
            {ok, "h1f1"}
    end;
is_castling(_, _, _, _) ->
    false.

%% ----------------------------------------------------------------------
%% chess notation

%% @doc
-spec notation(Chessman :: echessd_game:chessman(),
               Ply :: echessd_game:ply(),
               Capture :: entry(), NewBoard :: board(),
               GameStatus :: echessd_game:status(),
               History :: echessd_game:history()) ->
                      ChessNotation :: nonempty_string().
notation(Chessman, Ply, Capture, NewBoard, GameStatus, History) ->
    {SrcIndex, DstIndex, _Tail} = ply_dec(Ply),
    {Color, ChessmanType} = Chessman,
    {PlyCoords, _PlyInfo} = Ply,
    lists:flatten(
      [case {Chessman, PlyCoords} of
           {?wking, "e1g1" ++ _} -> "0-0";
           {?wking, "e1c1" ++ _} -> "0-0-0";
           {?bking, "e8g8" ++ _} -> "0-0";
           {?bking, "e8c8" ++ _} -> "0-0-0";
           _ ->
               {_, NewChessmanType} = getcell(NewBoard, DstIndex),
               [chessman_type_to_notation(ChessmanType),
                ind_enc(SrcIndex),
                if_then_else(Capture == ?empty, "-", "x"),
                ind_enc(DstIndex),
                if ChessmanType == ?pawn andalso
                   NewChessmanType /= ?pawn ->
                        chessman_type_to_notation(NewChessmanType);
                   true -> ""
                end]
       end,
       case GameStatus of
           none ->
               EnemyColor = not_color(Color),
               EnemyKingIndex = whereis_the_king(History, EnemyColor),
               case cell_attackers_count(
                      NewBoard, EnemyKingIndex, EnemyColor) of
                   0 -> "";
                   1 -> "+";
                   2 -> "++"
               end;
           checkmate ->
               "#";
           {draw, stalemate} ->
               "="
       end]).

%% @doc
-spec chessman_type_to_notation(echessd_game:chessman_type()) ->
                                       string().
chessman_type_to_notation(?king) ->
    "K";
chessman_type_to_notation(?queen) ->
    "Q";
chessman_type_to_notation(?rook) ->
    "R";
chessman_type_to_notation(?knight) ->
    "N";
chessman_type_to_notation(?bishop) ->
    "B";
chessman_type_to_notation(?pawn) ->
    "".

%% ----------------------------------------------------------------------
%% en passant

%% @doc
-spec is_en_passant(History :: echessd_game:history(),
                    OpponentPawnCoord :: coord()) ->
                           boolean().
is_en_passant([_ | _] = History, {C2, R2} = OppPawnCoord) ->
    case ply_dec(lists:last(History)) of
        {{C2, R1}, OppPawnCoord, _PrevPlyCoordsTail}
          when abs(abs(R1) - abs(R2)) == 2 ->
            true;
        _ ->
            false
    end;
is_en_passant(_, _) ->
    false.

%% @doc
-spec is_en_passant_simple(Board :: board(),
                           Index1 :: coord(), Index2 :: coord(),
                           SrcChessman :: entry(),
                           DstChessman :: entry()) ->
                                  boolean().
is_en_passant_simple(Board, {IC1, IR1}, {IC2, _IR2},
                     {C, ?pawn}, ?empty) when abs(IC2 - IC1) == 1 ->
    EnemyPawnIndex = {IC2, IR1},
    EnemyPawn = {not_color(C), ?pawn},
    case getcell(Board, EnemyPawnIndex) == EnemyPawn of
        true ->
            {ok, EnemyPawnIndex, EnemyPawn};
        false ->
            false
    end;
is_en_passant_simple(_, _, _, _, _) ->
    false.

%% ----------------------------------------------------------------------
%% low level tools
%% ----------------------------------------------------------------------

%% @doc
-spec is_promotion(SrcIndex :: coord(), Chessman :: entry(),
                   PlyCoordsTail :: string()) ->
                          {ok, ChessmanType :: echessd_game:chessman_type()} |
                          false.
is_promotion({_IC2, 1 = _IR2} = _I2, ?bpawn = _SrcChm, PlyCoordsTail) ->
    {ok, promotion_dec(PlyCoordsTail)};
is_promotion({_IC2, 8 = _IR2} = _I2, ?wpawn = _SrcChm, PlyCoordsTail) ->
    {ok, promotion_dec(PlyCoordsTail)};
is_promotion(_, _, _) ->
    false.

%% @doc
-spec is_empty_or_enemy(Board :: board(), MyColor :: echessd_game:color(),
                        Coord :: coord()) ->
                               (Coord :: coord()) | ?null.
is_empty_or_enemy(Board, MyColor, Coord) ->
    case getcell(Board, Coord) of
        ?empty ->
            Coord;
        {Color, _} when Color /= MyColor ->
            Coord;
        _ ->
            ?null
    end.

%% @doc
-spec free_cells_until_enemy(Board :: board(),
                             MyColor :: echessd_game:color(),
                             Start :: coord(),
                             Step :: step()) ->
                                    [FreeCell :: coord()].
free_cells_until_enemy(Board, MyColor, Start, Step) ->
    case ind_inc(Start, Step) of
        ?null ->
            [];
        Crd ->
            case getcell(Board, Crd) of
                ?empty ->
                    [Crd |
                     free_cells_until_enemy(
                       Board, MyColor, Crd, Step)];
                {MyColor, _} ->
                    [];
                _ ->
                    [Crd]
            end
    end.

%% @doc
-spec is_cell_attacked(Board :: board(), CellCoord :: coord(),
                       EnemyColor :: echessd_game:color()) ->
                              boolean().
is_cell_attacked(Board, CellCoord, EnemyColor) ->
    cell_attackers_count(Board, CellCoord, not_color(EnemyColor)) > 0.

%% @doc
-spec cell_attackers_count(Board :: board(), Index :: coord(),
                           MyColor :: echessd_game:color()) -> 0..2.
cell_attackers_count(Board, Index, Color) ->
    EnemyColor = not_color(Color),
    AttackingKnights = attacking_knights(Board, Index, EnemyColor),
    SearchDirections =
        [{-1, -1}, {-1, 0}, {-1, 1}, {0, -1},
         {0, 1}, {1, -1}, {1, 0}, {1, 1}],
    cell_attackers_loop(
      Board, Index, EnemyColor, SearchDirections, AttackingKnights).

%% @doc
-spec cell_attackers_loop(Board :: board(), Index :: coord(),
                          EnemyColor :: echessd_game:color(),
                          SearchDirections :: [step()],
                          Found :: non_neg_integer()) ->
                                 0..2.
cell_attackers_loop(_Board, _Index, _EnemyColor, _SearchDirections, Found)
  when Found >= 2 ->
    2;
cell_attackers_loop(_Board, _Index, _EnemyColor, [], Found) ->
    Found;
cell_attackers_loop(Board, Index, EnemyColor, [SearchDirection | Tail],
                    Found) ->
    {DC, DR} = SearchDirection,
    case find_enemy(Board, Index, SearchDirection, EnemyColor) of
        {?pawn, 1} when EnemyColor == ?black, abs(DC) == 1, DR == 1 ->
            cell_attackers_loop(Board, Index, EnemyColor, Tail, Found + 1);
        {?pawn, 1} when EnemyColor == ?white, abs(DC) == 1, DR == -1 ->
            cell_attackers_loop(Board, Index, EnemyColor, Tail, Found + 1);
        {?king, 1} ->
            cell_attackers_loop(Board, Index, EnemyColor, Tail, Found + 1);
        {?queen, _} ->
            cell_attackers_loop(Board, Index, EnemyColor, Tail, Found + 1);
        {?bishop, _} when abs(DC) == abs(DR) ->
            cell_attackers_loop(Board, Index, EnemyColor, Tail, Found + 1);
        {?rook, _} when abs(DC) /= abs(DR) ->
            cell_attackers_loop(Board, Index, EnemyColor, Tail, Found + 1);
        _ ->
            cell_attackers_loop(Board, Index, EnemyColor, Tail, Found)
    end.

%% @doc
-spec attacking_knights(Board :: board(), Index :: coord(),
                        EnemyColor :: echessd_game:color()) -> 0..2.
attacking_knights(Board, Index, EnemyColor) ->
    attacking_knights_loop(
      Board, Index, knight_steps(), EnemyColor, _Found = 0).

%% @doc
-spec knight_steps() -> [step()].
knight_steps() ->
    [{1, 2}, {-1, 2}, {1, -2}, {-1, -2},
     {2, 1}, {-2, 1}, {2, -1}, {-2, -1}].

%% @doc
-spec attacking_knights_loop(Board :: board(), Index :: coord(),
                             KnightSteps :: [step()],
                             EnemyColor :: echessd_game:color(),
                             Found :: 0..2) -> 0..2.
attacking_knights_loop(_Board, _Index, [], _EnemyColor, Found) ->
    Found;
attacking_knights_loop(Board, Index, [Step | Tail], EnemyColor, Found) ->
    case getcell(Board, ind_inc(Index, Step)) of
        {EnemyColor, ?knight} when Found >= 1 ->
            2;
        {EnemyColor, ?knight} ->
            attacking_knights_loop(Board, Index, Tail, EnemyColor, Found + 1);
        _ ->
            attacking_knights_loop(Board, Index, Tail, EnemyColor, Found)
    end.

%% @doc
-spec find_enemy(Board :: board(), Index :: coord(), Step :: step(),
                 EnemyColor :: echessd_game:color()) ->
                        (EnemyChessman :: echessd_game:chessman()) |
                        undefined.
find_enemy(Board, I, Step, EnemyColor) ->
    find_enemy(Board, I, Step, EnemyColor, 1).

%% @doc
-spec find_enemy(Board :: board(), Index :: coord(), Direction :: step(),
                 EnemyColor :: echessd_game:color(),
                 Distance :: pos_integer()) ->
                        (EnemyChessman :: echessd_game:chessman()) |
                        undefined.
find_enemy(Board, Index, Direction, EnemyColor, Distance) ->
    Index2 = ind_inc(Index, Direction),
    case getcell(Board, Index2) of
        {EnemyColor, ChessmanType} ->
            {ChessmanType, Distance};
        ?empty ->
            find_enemy(Board, Index2, Direction, EnemyColor, Distance + 1);
        _ ->
            %% board end reached or
            %% friendly chessman found
            undefined
    end.

%% @doc Return the king location.
-spec whereis_the_king(History :: echessd_game:history(),
                       Color :: echessd_game:color()) ->
                              Coord :: coord().
whereis_the_king(History, Color) ->
    whereis_the_king_(History, king_start_coord(Color)).

%% @doc
-spec whereis_the_king_(History :: echessd_game:history(),
                        KingCoord :: coord()) ->
                               FinalKingCoord :: coord().
whereis_the_king_([], Coord) ->
    Coord;
whereis_the_king_([Ply | Tail], Coord) ->
    case ply_dec(Ply) of
        {Coord, NewCoord, _Tail} ->
            whereis_the_king_(Tail, NewCoord);
        {_SrcIndex, _DstIndex, _Tail} ->
            whereis_the_king_(Tail, Coord)
    end.

%% @doc
-spec getcell(Board :: board(), Coord :: coord()) ->
                  (Entry :: entry()) | ?null.
getcell(Board, {C, R})
  when R >= 1 andalso R =< 8 andalso
       C >= 1 andalso C =< 8 ->
    element(C, element(9 - R, Board));
getcell(_, _) ->
    ?null.

%% @doc
-spec setcell(Board :: board(), Coord :: coord(), NewEntry :: entry()) ->
                     NewBoard :: board().
setcell(Board, {C, R}, NewEntry) ->
    Row = element(9 - R, Board),
    NewRow = setelement(C, Row, NewEntry),
    setelement(9 - R, Board, NewRow).

%% @doc
-spec ply_dec(Ply :: echessd_game:ply()) ->
                     {SrcIndex :: coord(), DstIndex :: coord(),
                      PlyCoordsTail :: string()}.
ply_dec({[A, B, C, D | PlyCoordsTail], _PlyInfo})
  when is_integer(A), is_integer(B), is_integer(C), is_integer(D) ->
    {{_, _} = ind_dec(A, B), {_, _} = ind_dec(C, D), PlyCoordsTail}.

%% @doc
-spec ind_dec(HalfPly :: nonempty_string()) -> Coord :: coord().
ind_dec([C, R]) ->
    ind_dec(C, R).

%% @doc
-spec ind_dec(BoardLetter :: 97..104, %% $a..$h
              BoardNumber :: 49..56   %% $1..$8
                             ) -> (Coord :: coord()) | ?null.
ind_dec(C, R) ->
    Crd = {C - $a + 1, R - $1 + 1},
    case ind_ok(Crd) of
        true ->
            Crd;
        _ ->
            ?null
    end.

%% @doc
-spec ind_enc(Coord :: coord()) -> HalfPly :: nonempty_string().
ind_enc({C, R}) ->
    [$a + C - 1, $1 + R - 1].

%% @doc
-spec ind_inc(Coord :: coord(), Step :: step()) ->
                     (NewCoord :: coord()) | ?null.
ind_inc({C, R}, {CS, RS}) ->
    Crd = {C + CS, R + RS},
    case ind_ok(Crd) of
        true ->
            Crd;
        _ ->
            ?null
    end.

%% @doc
-spec ind_row(Coord :: coord()) -> RowIndex :: 1..8.
ind_row({_, R}) ->
    R.

%% @doc
-spec ind_ok(coord() | any()) -> boolean().
ind_ok({C, R})
  when C >= 1 andalso C =< 8 andalso
       R >= 1 andalso R =< 8 ->
    true;
ind_ok(_) ->
    false.

%% @doc
-spec not_color(MyColor :: echessd_game:color()) ->
                       OpponentColor :: echessd_game:color().
not_color(MyColor) ->
    hd([?white, ?black] -- [MyColor]).

%% @doc
-spec promotion_dec(PlyTail :: string()) ->
                           echessd_game:chessman_type().
promotion_dec([$r | _]) -> ?rook;
promotion_dec([$k | _]) -> ?knight;
promotion_dec([$s | _]) -> ?knight;
promotion_dec([$h | _]) -> ?knight;
promotion_dec([$b | _]) -> ?bishop;
promotion_dec([$q | _]) -> ?queen;
promotion_dec([]) -> ?queen.

%% @doc
-spec if_then_else(Expression :: boolean(),
                   OnTrue :: any(), OnFalse :: any()) -> any().
if_then_else(true, OnTrue, _OnFalse) ->
    OnTrue;
if_then_else(false, _OnTrue, OnFalse) ->
    OnFalse.
