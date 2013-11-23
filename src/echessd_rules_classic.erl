%%% @doc
%%% Classic chess rules implementation.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 22 Jan 2012
%%% @copyright 2012, Aleksey Morarash

-module(echessd_rules_classic).

-export(
   [new/0,
    make_ply/4,
    possibles/1,
    move_chessman/2,
    can_move/3,
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
        {ColumnDelta :: -8..8, RowDelta :: -8..8}.

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

%% @doc Apply user's move.
-spec make_ply(Board :: board(), TurnColor :: echessd_game:color(),
               Ply :: echessd_game:ply(),
               History :: echessd_game:history()) ->
                      {ok, NewBoard :: board(),
                       NewHistory :: echessd_game:history(),
                       GameStatus :: echessd_game:status()} |
                      {error, Reason :: any()}.
make_ply(Board, TurnColor, Ply, History) ->
    try
        make_ply_(Board, TurnColor, Ply, History)
    catch
        _:{error, _} = ExcError -> ExcError;
        Type:Reason ->
            {error, {Type, Reason, erlang:get_stacktrace()}}
    end.

%% @doc Return a list of all available valid moves.
%% The function used only for next move highlighting
%% in the user interface.
-spec possibles(History :: echessd_game:history()) ->
                       Plies :: [echessd_game:ply_coords()].
possibles(History) ->
    Color = echessd_game:turn_color_by_history(History),
    Board =
        lists:foldl(
          fun(Ply, Acc) ->
                  element(1, move_chessman(Acc, Ply))
          end, new(), History),
    lists:flatmap(
      fun(Col) ->
              lists:flatmap(
                fun(Row) ->
                        possibles_from(
                          Board, Color, {Col, Row},
                          History)
                end, lists:seq(1, 8))
      end, lists:seq(1, 8)).

%% @doc
-spec possibles_from(Board :: board(),
                     Color :: echessd_game:color(),
                     Index :: coord(),
                     History :: echessd_game:history()) ->
                            [echessd_game:ply_coords()].
possibles_from(Board, Color, I1, History) ->
    case cell(Board, I1) of
        {Color, ChessmanType} ->
            Possibles =
                possible(
                  Board, I1, Color, ChessmanType, History),
            lists:flatmap(
              fun(I2) ->
                      IsValid =
                          possibles_(
                            Board, Color, ChessmanType,
                            I1, I2, History),
                      if IsValid -> [ind_enc(I1) ++ ind_enc(I2)];
                         true -> []
                      end
              end, Possibles);
        _ -> []
    end.

%% @doc
-spec possibles_(Board :: board(),
                 Color :: echessd_game:color(),
                 ChessmanType :: echessd_game:chessman_type(),
                 SrcIndex :: coord(),
                 DstIndex :: coord(),
                 History :: echessd_game:history()) ->
                        boolean().
possibles_(Board, Color, ChessmanType, I1, I2, History) ->
    try
        {ok, _NewBoard, _Capture} =
            try_possible(Board, Color, ChessmanType, I1, I2, [], History),
        true
    catch _:_ ->
            false
    end.

%% @doc Move the chessman.
-spec move_chessman(Board :: board(),
                    Ply :: echessd_game:ply() | echessd_game:ply_coords()) ->
                           {NewBoard :: board(), Capture :: entry()}.
move_chessman(Board, Ply) ->
    {I1, I2, Tail} = ply_dec(Ply),
    move_chessman(Board, I1, I2, Tail).

%% @doc
-spec move_chessman(Board :: board(), SrcIndex :: coord(),
                    DstIndex :: coord(), Tail :: string()) ->
                           {NewBoard :: board(), Capture :: entry()}.
move_chessman(Board, I1, I2, Tail) ->
    F1 = cell(Board, I1),
    F2 = cell(Board, I2),
    case is_en_passant_simple(Board, I1, I2, F1, F2) of
        {ok, EnemyPawnIndex, EnemyPawn} ->
            Board2 = setcell(Board, EnemyPawnIndex, ?empty),
            {Board3, ?empty} =
                move_chessman_normal(Board2, I1, I2, F1, F2),
            {Board3, EnemyPawn};
        _ ->
            case is_promotion(I2, F1, Tail) of
                {ok, ChessmanType} ->
                    {Color, _} = F1,
                    move_chessman_normal(
                      Board, I1, I2,
                      {Color, ChessmanType}, F2);
                _ ->
                    case is_castling(I1, I2, F1, F2) of
                        {ok, RookPly} ->
                            {Board2, _} =
                                move_chessman_normal(
                                  Board, I1, I2, F1, F2),
                            move_chessman(Board2, RookPly);
                        _ ->
                            move_chessman_normal(
                              Board, I1, I2, F1, F2)
                    end
            end
    end.

%% @doc
-spec move_chessman_normal(Board :: board(), I1 :: any(), I2 :: any(),
                           F1 :: any(), F2 :: any()) ->
                                  {NewBoard :: board(),
                                   Capture :: entry()}.
move_chessman_normal(Board, I1, I2, F1, F2) ->
    {setcell(setcell(Board, I1, ?empty), I2, F1), F2}.

%% @doc Check if valid turn is present for the color.
-spec can_move(Board :: board(), Color :: echessd_game:color(),
               History :: echessd_game:history()) -> boolean().
can_move(Board, Color, History) ->
    Seq = lists:seq(1, 8),
    lists:any(
      fun(I1) ->
              case cell(Board, I1) of
                  {Color, ChessmanType} ->
                      Possibles =
                          possible(
                            Board, I1, Color,
                            ChessmanType, History),
                      lists:any(
                        fun(I2) ->
                                try
                                    {ok, _NewBoard, _Capture} =
                                        try_possible(
                                          Board, Color, ChessmanType,
                                          I1, I2, "q", History),
                                    true
                                catch _:_ -> false
                                end
                        end, Possibles);
                  _ -> false
              end
      end, [{C, R} || C <- Seq, R <- Seq]).

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

%% @doc Return game over status.
-spec gameover_status(Board :: board(), Color :: echessd_game:color(),
                      History :: echessd_game:history()) ->
                             GameStatus :: echessd_game:status().
gameover_status(Board, Color, History) ->
    case can_move(Board, Color, History) of
        true -> none;
        _ ->
            KingIndex = whereis_my_king(History, Color),
            case cell_attackers_count(
                   Board, KingIndex, Color) of
                0 -> {draw, stalemate};
                _ -> checkmate
            end
    end.

%% @doc
-spec chessman_type_to_notation(echessd_game:chessman_type()) ->
                                       string().
chessman_type_to_notation(?king) -> "K";
chessman_type_to_notation(?queen) -> "Q";
chessman_type_to_notation(?rook) -> "R";
chessman_type_to_notation(?knight) -> "N";
chessman_type_to_notation(?bishop) -> "B";
chessman_type_to_notation(?pawn) -> "".

-define(null, '*null').

%% @doc
-spec make_ply_(Board :: board(), TurnColor :: echessd_game:color(),
                Ply :: echessd_game:ply(),
                History :: echessd_game:history()) ->
                       {ok, NewBoard :: board(),
                        NewHistory :: echessd_game:history(),
                        GameStatus :: echessd_game:status()}.
make_ply_(Board, TurnColor, {Coords, Meta} = Ply, History) ->
    {I1, I2, Tail} = ply_dec(Ply),
    {_, ChessmanType} = Chessman =
        case cell(Board, I1) of
            ?empty -> throw({error, cell_is_empty});
            {TurnColor, _} = Chessman0 -> Chessman0;
            {_, _} -> throw({error, not_your_chessman})
        end,
    case cell(Board, I2) of
        {TurnColor, _} -> throw({error, friendly_fire});
        {_, ?king} -> throw({error, cannot_take_king});
        _ -> ok
    end,
    Possible = possible(Board, I1, TurnColor, ChessmanType, History),
    case lists:member(I2, Possible) of
        true ->
            {ok, NewBoard, Capture} =
                try_possible(
                  Board, TurnColor, ChessmanType,
                  I1, I2, Tail, History),
            NotationBase =
                case {Chessman, Coords} of
                    {?wking, "e1g1" ++ _} -> "0-0";
                    {?wking, "e1c1" ++ _} -> "0-0-0";
                    {?bking, "e8g8" ++ _} -> "0-0";
                    {?bking, "e8c8" ++ _} -> "0-0-0";
                    _ ->
                        StrPlyType =
                            case Capture of
                                ?empty -> "-";
                                _ -> "x"
                            end,
                        NewChessmanType = element(2, cell(NewBoard, I2)),
                        StrPromotion =
                            if ChessmanType == ?pawn andalso
                               NewChessmanType /= ?pawn ->
                                    chessman_type_to_notation(
                                      NewChessmanType);
                               true -> ""
                            end,
                        chessman_type_to_notation(ChessmanType) ++
                            ind_enc(I1) ++ StrPlyType ++
                            ind_enc(I2) ++ StrPromotion
                end,
            OppColor = not_color(TurnColor),
            OppKingIndex =
                whereis_my_king(History, OppColor),
            GameStatus =
                gameover_status(NewBoard, OppColor, History ++ [Ply]),
            NotationExtra =
                case GameStatus of
                    none ->
                        AttackersCount =
                            cell_attackers_count(
                              NewBoard, OppKingIndex, OppColor),
                        if AttackersCount == 1 -> "+";
                           AttackersCount == 2 -> "++";
                           true -> ""
                        end;
                    checkmate -> "#";
                    {draw, stalemate} -> "="
                end,
            FinalPly =
                {Coords,
                 echessd_lib:proplist_replace(
                   Meta, [{notation, NotationBase ++ NotationExtra}])},
            {ok, NewBoard, History ++ [FinalPly], GameStatus};
        _ ->
            throw({error, badmove})
    end.

%% @doc
-spec try_possible(Board :: board(), Color :: echessd_game:color(),
                   ChessmanType :: echessd_game:chessman_type(),
                   I1 :: any(), I2 :: any(), Tail :: any(),
                   History :: echessd_game:history()) ->
                          {ok, NewBoard :: board(), Capture :: entry()}.
try_possible(Board, Color, ChessmanType, I1, I2, Tail, History) ->
    {NewBoard, Capture} = move_chessman(Board, I1, I2, Tail),
    KingIndex =
        if ChessmanType == ?king ->
                I2;
           true ->
                whereis_my_king(History, Color)
        end,
    case cell_attackers_count(NewBoard, KingIndex, Color) of
        0 ->
            {ok, NewBoard, Capture};
        _ ->
            throw({error, check})
    end.

%% @doc
-spec possible(Board :: board(), Index :: any(),
               Color :: echessd_game:color(),
               ChessmanType :: echessd_game:chessman_type(),
               History :: echessd_game:history()) ->
                      [any()].
possible(B, I, C, ChessmanType, History) ->
    lists:usort(
      lists:flatten(
        possible_(B, I, C, ChessmanType, History))) -- [?null].

%% @doc
-spec possible_(Board :: board(), Index :: any(),
                Color :: echessd_game:color(),
                ChessmanType :: echessd_game:chessman_type(),
                History :: echessd_game:history()) ->
                       [any()].
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
    case cell(B, F1) of
        ?empty ->
            [F1] ++
                case ind_row(I) of
                    StartRow ->
                        case cell(B, F2) of
                            ?empty -> [F2];
                            _ -> []
                        end;
                    _ -> []
                end;
        _ -> []
    end ++
        case cell(B, FL) of
            {C, _} -> [];
            {_, _} -> [FL];
            _ -> []
        end ++
        case cell(B, FR) of
            {C, _} -> [];
            {_, _} -> [FR];
            _ -> []
        end ++
        case cell(B, EnPassL) of
            OppPawn ->
                case is_en_passant(History, EnPassL) of
                    true ->
                        [ind_inc(I, {-1, Direction})];
                    _ -> []
                end;
            _ -> []
        end ++
        case cell(B, EnPassR) of
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
        possible_castlings(B, C, History).

%% @doc
-spec possible_castlings(Board :: board(),
                         Color :: echessd_game:color(),
                         History :: echessd_game:history()) ->
                                [any()].
possible_castlings(Board, Color, History) ->
    case is_king_have_been_moved(History, Color) of
        true ->
            [];
        _ ->
            KingStart =
                ind_dec(
                  if Color == ?white -> "e1";
                     true -> "e8"
                  end),
            PossibleCastlings =
                %% castling long
                case search_rook(Board, KingStart, {-1, 0}, Color) of
                    {1, _} = LongRookC ->
                        case is_rook_have_been_moved(
                               History, LongRookC) of
                            true -> [];
                            _ ->
                                case cell_attackers_count(
                                       Board, ind_inc(KingStart, {-1, 0}),
                                       Color) of
                                    0 -> [ind_inc(KingStart, {-2, 0})];
                                    _ -> []
                                end
                        end;
                    _ -> []
                end ++
                %% castling short
                case search_rook(Board, KingStart, {1, 0}, Color) of
                    {8, _} = ShortRookC ->
                        case is_rook_have_been_moved(
                               History, ShortRookC) of
                            true -> [];
                            _ ->
                                case cell_attackers_count(
                                       Board, ind_inc(KingStart, {1, 0}),
                                       Color) of
                                    0 -> [ind_inc(KingStart, {2, 0})];
                                    _ -> []
                                end
                        end;
                    _ -> []
                end,
            case PossibleCastlings of
                [_ | _] ->
                    %% is there is check?
                    case cell_attackers_count(
                           Board, KingStart, Color) of
                        0 -> PossibleCastlings;
                        _ -> []
                    end;
                _ -> []
            end
    end.

%% @doc
-spec search_rook(Board :: board(), Index :: any(),
                  Step :: any(), Color :: echessd_game:color()) ->
                         (RookIndex :: any()) | ?null.
search_rook(Board, I, Step, Color) ->
    I2 = ind_inc(I, Step),
    case cell(Board, I2) of
        {Color, ?rook} ->
            I2;
        ?empty ->
            search_rook(Board, I2, Step, Color);
        _ ->
            ?null
    end.

%% ----------------------------------------------------------------------
%% low level tools
%% ----------------------------------------------------------------------

%% @doc
-spec is_en_passant(History :: echessd_game:history(),
                    OpponentPawnCoord :: coord()) ->
                           boolean().
is_en_passant([_ | _] = History, {C2, R2} = OppPawnCoord) ->
    [A, B, C, D | _] = strip_ply_meta(lists:last(History)),
    case ind_dec(C, D) of
        OppPawnCoord ->
            case ind_dec(A, B) of
                {C2, R1} when abs(abs(R1) - abs(R2)) == 2 ->
                    true;
                _ -> false
            end;
        _ -> false
    end;
is_en_passant(_, _) ->
    false.

%% @doc
-spec is_en_passant_simple(Board :: board(),
                           Index1 :: coord(), Index2 :: coord(),
                           SrcChessman :: entry(),
                           DstChessman :: entry()) ->
                                  boolean().
is_en_passant_simple(Board, {IC1, IR1} = _I1, {IC2, _IR2} = _I2,
                     {C, ?pawn} = _SrcChm, ?empty = _DstChm)
  when abs(IC2 - IC1) == 1 ->
    EnemyPawnIndex = {IC2, IR1},
    EnemyPawn = {not_color(C), ?pawn},
    case cell(Board, EnemyPawnIndex) == EnemyPawn of
        true ->
            {ok, EnemyPawnIndex, EnemyPawn};
        _ ->
            false
    end;
is_en_passant_simple(_, _, _, _, _) ->
    false.

%% @doc
-spec is_promotion(Index1 :: coord(), Chessman :: entry(),
                   PlyTail :: string()) ->
                          {ok, ChessmanType :: echessd_game:chessman_type()} |
                          false.
is_promotion({_IC2, 1 = _IR2} = _I2, ?bpawn = _SrcChm, PlyTail) ->
    {ok, promotion_dec(PlyTail)};
is_promotion({_IC2, 8 = _IR2} = _I2, ?wpawn = _SrcChm, PlyTail) ->
    {ok, promotion_dec(PlyTail)};
is_promotion(_, _, _) ->
    false.

%% @doc
-spec is_castling(SrcIndex :: coord(), DstIndex :: coord(),
                  SrcChessman :: echessd_game:chessman(),
                  DstChessman :: entry()) ->
                         {ok, nonempty_string()} |
                         false.
is_castling({IC1, IR} = _I1, {IC2, IR} = _I2,
            {C, ?king} = _SrcChm, ?empty = _DstChm)
  when abs(IC2 - IC1) == 2
       andalso
       ((C == ?black andalso IR == 8 andalso IC1 == 5)
        orelse
          (C == ?white andalso IR == 1 andalso IC1 == 5)) ->
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

%% @doc
-spec is_empty_or_enemy(Board :: board(), MyColor :: echessd_game:color(),
                        Coord :: coord()) ->
                               (Coord :: coord()) | ?null.
is_empty_or_enemy(Board, MyColor, Coord) ->
    case cell(Board, Coord) of
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
            case cell(Board, Crd) of
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
-spec knight_steps() -> [step()].
knight_steps() ->
    [{1, 2}, {-1, 2}, {1, -2}, {-1, -2},
     {2, 1}, {-2, 1}, {2, -1}, {-2, -1}].

%% @doc
-spec cell_attackers_count(Board :: board(), Index :: coord(),
                           MyColor :: echessd_game:color()) ->
                                  non_neg_integer().
cell_attackers_count(Board, I, Color) ->
    EnemyColor = not_color(Color),
    lists:sum(
      lists:map(
        fun(Step) ->
                case cell(Board, ind_inc(I, Step)) of
                    {EnemyColor, ?knight} -> 1;
                    _ -> 0
                end
        end, knight_steps()) ++
          lists:map(
            fun({DC, DR} = Step) ->
                    case find_enemy(
                           Board, I, Step, EnemyColor) of
                        {?pawn, 1}
                          when EnemyColor == ?black
                               andalso abs(DC) == 1
                               andalso DR == 1 -> 1;
                        {?pawn, 1}
                          when EnemyColor == ?white
                               andalso abs(DC) == 1
                               andalso DR == -1 -> 1;
                        {?king, 1} -> 1;
                        {?queen, _} -> 1;
                        {?bishop, _}
                          when abs(DC) == abs(DR) -> 1;
                        {?rook, _}
                          when abs(DC) /= abs(DR) -> 1;
                        _ -> 0
                    end
            end,
            [{-1, -1}, {-1, 0}, {-1, 1}, {0, -1},
             {0, 1}, {1, -1}, {1, 0}, {1, 1}])).

%% @doc
-spec find_enemy(Board :: board(), Index :: coord(), Step :: step(),
                 EnemyColor :: echessd_game:color()) ->
                        (EnemyChessman :: echessd_game:chessman()) |
                        undefined.
find_enemy(Board, I, Step, EnemyColor) ->
    find_enemy(Board, I, Step, EnemyColor, 1).

%% @doc
-spec find_enemy(Board :: board(), Index :: coord(), Step :: step(),
                 EnemyColor :: echessd_game:color(),
                 Distance :: pos_integer()) ->
                        (EnemyChessman :: echessd_game:chessman()) |
                        undefined.
find_enemy(Board, I, Step, EnemyColor, Distance) ->
    I2 = ind_inc(I, Step),
    case cell(Board, I2) of
        {EnemyColor, ChessmanType} ->
            {ChessmanType, Distance};
        ?empty ->
            find_enemy(Board, I2, Step, EnemyColor, Distance + 1);
        _ ->
            %% board end reached or
            %% friendly chessman found
            undefined
    end.

%% @doc
-spec is_king_have_been_moved(History :: echessd_game:history(),
                              Color :: echessd_game:color()) ->
                                     boolean().
is_king_have_been_moved(History, ?white) ->
    lists:any(
      fun(Ply) ->
              lists:prefix("e1", strip_ply_meta(Ply))
      end, History);
is_king_have_been_moved(History, ?black) ->
    lists:any(
      fun(Ply) ->
              lists:prefix("e8", strip_ply_meta(Ply))
      end, History).

%% @doc
-spec is_rook_have_been_moved(History :: echessd_game:history(),
                              Coord :: coord()) ->
                                     boolean().
is_rook_have_been_moved(History, C) ->
    lists:any(
      fun(Ply) ->
              lists:prefix(ind_enc(C), strip_ply_meta(Ply))
      end, History).

%% @doc
-spec whereis_my_king(History :: echessd_game:history(),
                      Color :: echessd_game:color()) ->
                             Coord :: coord().
whereis_my_king(History, ?white) ->
    whereis_my_king(History, "e1");
whereis_my_king(History, ?black) ->
    whereis_my_king(History, "e8");
whereis_my_king([{Ply, _Meta} | Tail], Start) ->
    whereis_my_king([Ply | Tail], Start);
whereis_my_king([[A, B, C, D | _] | Tail], [A, B]) ->
    whereis_my_king(Tail, [C, D]);
whereis_my_king([_ | Tail], Pos) ->
    whereis_my_king(Tail, Pos);
whereis_my_king(_, [A, B]) ->
    ind_dec(A, B).

%% @doc
-spec cell(Board :: board(), Coord :: coord()) ->
                  (Entry :: entry()) | ?null.
cell(Board, {C, R})
  when R >= 1 andalso R =< 8 andalso
       C >= 1 andalso C =< 8 ->
    element(C, element(9 - R, Board));
cell(_, _) ->
    ?null.

%% @doc
-spec setcell(Board :: board(), Coord :: coord(), NewEntry :: entry()) ->
                     NewBoard :: board().
setcell(Board, {C, R}, NewEntry) ->
    Row = element(9 - R, Board),
    NewRow = setelement(C, Row, NewEntry),
    setelement(9 - R, Board, NewRow).

%% @doc
-spec ply_dec(Ply :: echessd_game:ply() | echessd_game:ply_coords()) ->
                     {SrcCoord :: coord(), DstCoord :: coord(),
                      PlyTail :: string()}.
ply_dec({Ply, _Meta}) ->
    ply_dec(Ply);
ply_dec([A, B, C, D | Tail])
  when is_integer(A), is_integer(B),
       is_integer(C), is_integer(D) ->
    case {ind_dec(A, B), ind_dec(C, D)} of
        {{_,_} = C1, {_,_} = C2} ->
            {C1, C2, Tail};
        _ ->
            throw({error, badmove})
    end;
ply_dec(_) ->
    throw({error, badmove}).

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
-spec strip_ply_meta(Ply :: echessd_game:ply()) ->
                            nonempty_string().
strip_ply_meta({[_, _, _, _ | _] = Ply, _Meta}) ->
    Ply;
strip_ply_meta([_, _, _, _ | _] = Other) ->
    Other.
