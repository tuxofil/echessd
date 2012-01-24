%%%-------------------------------------------------------------------
%%% File    : echessd_rules_classic.erl
%%% Author  : Aleksey Morarash <aleksey.morarash@gmail.com>
%%% Created : 22 Jan 2012
%%% License : FreeBSD
%%% Description : Classic chess rules implementation
%%%
%%%-------------------------------------------------------------------

-module(echessd_rules_classic).

-export([new/0,
         is_valid_move/4,
         move_figure/2,
         transpose/1
        ]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Creates chess board with all figures at start point.
%% @spec new() -> echessd_game:echessd_board()
new() ->
    {{?brook,?bknight,?bbishop,?bqueen,?bking,?bbishop,?bknight,?brook}, %% 8
     {?bpawn,?bpawn,?bpawn,?bpawn,?bpawn,?bpawn,?bpawn,?bpawn}, %% 7
     {?empty,?empty,?empty,?empty,?empty,?empty,?empty,?empty}, %% 6
     {?empty,?empty,?empty,?empty,?empty,?empty,?empty,?empty}, %% 5
     {?empty,?empty,?empty,?empty,?empty,?empty,?empty,?empty}, %% 4
     {?empty,?empty,?empty,?empty,?empty,?empty,?empty,?empty}, %% 3
     {?wpawn,?wpawn,?wpawn,?wpawn,?wpawn,?wpawn,?wpawn,?wpawn}, %% 2
     {?wrook,?wknight,?wbishop,?wqueen,?wking,?wbishop,?wknight,?wrook}  %% 1
     %% a,b,c,d,e,f,g,h
    }.

%% @doc Checks if move is valid.
%% @spec is_valid_move(Board, TurnColor, Move, History) ->
%%                 ok | {error, Reason}
%%     Board = echessd_game:echessd_board(),
%%     TurnColor = echessd_game:echessd_color(),
%%     Move = echessd_game:echessd_move(),
%%     History = echessd_game:echessd_history(),
%%     Reason = term()
is_valid_move(Board, TurnColor, Move, History) ->
    try is_valid_move_(Board, TurnColor, Move, History) of
        ok -> ok;
        {error, _} = Error -> Error;
        Other -> {error, Other}
    catch
        _:{error, _} = ExcError -> ExcError;
        Type:Reason ->
            {error, {Type, Reason, erlang:get_stacktrace()}}
    end.

%% @doc Make figure move.
%% @spec move_figure(Board, Move) -> {NewBoard, Capture}
%%     Board = NewBoard = echessd_game:echessd_board(),
%%     Capture = echessd_game:echessd_figure()
move_figure(Board, Move) ->
    {C1, C2, Tail} = move_dec(Move),
    move_figure(Board, C1, C2, Tail).
move_figure(Board, C1, C2, Tail) ->
    F1 = cell(Board, C1),
    F2 = cell(Board, C2),
    case is_en_passant_simple(Board, C1, C2, F1, F2) of
        {ok, EnemyPawnIndex, EnemyPawn} ->
            Board2 = setcell(Board, EnemyPawnIndex, ?empty),
            {Board3, ?empty} =
                move_figure_normal(Board2, C1, C2, F1, F2),
            {Board3, EnemyPawn};
        _ ->
            case is_promotion(C2, F1, Tail) of
                {ok, FigureType} ->
                    {Color, _} = F1,
                    move_figure_normal(
                      Board, C1, C2,
                      {Color, FigureType}, ?empty);
                _ ->
                    case is_castling(C1, C2, F1, F2) of
                        {ok, RookMove} ->
                            {Board2, _} =
                                move_figure_normal(
                                  Board, C1, C2, F1, F2),
                            move_figure(Board2, RookMove);
                        _ ->
                            move_figure_normal(
                              Board, C1, C2, F1, F2)
                    end
            end
    end.
move_figure_normal(Board, C1, C2, F1, F2) ->
    {setcell(setcell(Board, C1, ?empty), C2, F1), F2}.

%% @doc Turns internal board representation at 180 degrees.
%% @spec transpose(Board) -> NewBoard
%%     GameType = echessd_game:echessd_game_type(),
%%     Board = NewBoard = echessd_game:echessd_board()
transpose(Board) ->
    list_to_tuple(
      lists:reverse(
        [list_to_tuple(
           lists:reverse(
             tuple_to_list(R))) ||
            R <- tuple_to_list(Board)])).

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

-define(null, '*null').

is_valid_move_(Board, TurnColor, Move, History) ->
    {C1, C2, Tail} = move_dec(Move),
    {MyColor, FigureType} =
        case cell(Board, C1) of
            ?empty -> throw({error, {cell_is_empty, C1}});
            {TurnColor, _} = Figure0 -> Figure0;
            {_, _} -> throw({error, not_your_figure})
        end,
    case cell(Board, C2) of
        {TurnColor, _} -> throw({error, friendly_fire});
        {_, ?king} -> throw({error, cannot_take_king});
        _ -> ok
    end,
    Possible = possible(Board, C1, MyColor, FigureType, History),
    case lists:member(C2, Possible) of
        true ->
            {Board2, _Capture} = move_figure(Board, C1, C2, Tail),
            MyKingIndex =
                if FigureType == ?king -> C2;
                   true ->
                        whereis_my_king(History, MyColor)
                end,
            case is_cell_under_attack(
                   Board2, MyKingIndex, MyColor) of
                true ->
                    throw({error, check});
                _ -> ok
            end;
        _ ->
            throw({error, badmove})
    end.

possible(B, F, C, FigType, History) ->
    lists:usort(
      lists:flatten(
        possible_(B, F, C, FigType, History))) -- [?null].
possible_(B, F, C, ?pawn, History) ->
    {StartRow, Direction} =
        if C == ?white -> {2, 1};
           true -> {7, -1}
        end,
    F1 = crd_inc(F, {0, Direction}),
    F2 = crd_inc(F, {0, Direction * 2}),
    FL = crd_inc(F, {-1, Direction}),
    FR = crd_inc(F, {1, Direction}),
    EnPassL = crd_inc(F, {-1, 0}),
    EnPassR = crd_inc(F, {1, 0}),
    OppPawn = {not_color(C), ?pawn},
    case cell(B, F1) of
        ?empty ->
            [F1] ++
                case crd_row(F) of
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
            {_, _} -> [FL];
            _ -> []
        end ++
        case cell(B, FR) of
            {_, _} -> [FR];
            _ -> []
        end ++
        case cell(B, EnPassL) of
            OppPawn ->
                case is_en_passant(History, EnPassL) of
                    true ->
                        [crd_inc(F, {-1, Direction})];
                    _ -> []
                end;
            _ -> []
        end ++
        case cell(B, EnPassR) of
            OppPawn ->
                case is_en_passant(History, EnPassR) of
                    true ->
                        [crd_inc(F, {1, Direction})];
                    _ -> []
                end;
            _ -> []
        end;
possible_(B, F, C, ?rook, _History) ->
    [free_cells_until_enemy(B, C, F, {0, 1}),
     free_cells_until_enemy(B, C, F, {0, -1}),
     free_cells_until_enemy(B, C, F, {1, 0}),
     free_cells_until_enemy(B, C, F, {-1, 0})];
possible_(B, F, C, ?bishop, _History) ->
    [free_cells_until_enemy(B, C, F, {-1, -1}),
     free_cells_until_enemy(B, C, F, {1, -1}),
     free_cells_until_enemy(B, C, F, {-1, 1}),
     free_cells_until_enemy(B, C, F, {1, 1})];
possible_(B, F, C, ?queen, History) ->
    [possible_(B, F, C, ?bishop, History),
     possible_(B, F, C, ?rook, History)];
possible_(B, F, C, ?knight, _History) ->
    [is_empty_or_enemy(B, C, crd_inc(F, Step)) ||
        Step <- knight_steps()];
possible_(B, F, C, ?king, History) ->
    [is_empty_or_enemy(B, C, crd_inc(F, {-1, -1})),
     is_empty_or_enemy(B, C, crd_inc(F, {-1, 0})),
     is_empty_or_enemy(B, C, crd_inc(F, {-1, 1})),
     is_empty_or_enemy(B, C, crd_inc(F, {0, -1})),
     is_empty_or_enemy(B, C, crd_inc(F, {0, 1})),
     is_empty_or_enemy(B, C, crd_inc(F, {1, -1})),
     is_empty_or_enemy(B, C, crd_inc(F, {1, 0})),
     is_empty_or_enemy(B, C, crd_inc(F, {1, 1}))] ++
        possible_castlings(B, C, History);
possible_(_, _, _, _, _) -> [].

possible_castlings(Board, Color, History) ->
    case is_king_have_been_moved(History, Color) of
        true -> [];
        _ ->
            KingStart =
                crd_dec(
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
                                case is_cell_under_attack(
                                       Board, crd_inc(KingStart, {-1, 0}),
                                       Color) of
                                    true -> [];
                                    _ ->
                                        [crd_inc(KingStart, {-2, 0})]
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
                                case is_cell_under_attack(
                                       Board, crd_inc(KingStart, {1, 0}),
                                       Color) of
                                    true -> [];
                                    _ ->
                                        [crd_inc(KingStart, {2, 0})]
                                end
                        end;
                    _ -> []
                end,
            case PossibleCastlings of
                [_ | _] ->
                    %% is there is check?
                    case is_cell_under_attack(
                           Board, KingStart, Color) of
                        true -> [];
                        _ -> PossibleCastlings
                    end;
                _ -> []
            end
    end.

search_rook(Board, I, Step, Color) ->
    I2 = crd_inc(I, Step),
    case cell(Board, I2) of
        {Color, ?rook} -> I2;
        ?empty -> search_rook(Board, I2, Step, Color);
        _ -> ?null
    end.

%% ----------------------------------------------------------------------
%% low level tools
%% ----------------------------------------------------------------------

is_en_passant([_ | _] = History, {C2, R2} = OppPawnCoord) ->
    [A, B, C, D] = lists:last(History),
    case crd_dec(C, D) of
        OppPawnCoord ->
            case crd_dec(A, B) of
                {C2, R1} when abs(abs(R1) - abs(R2)) == 2 ->
                    true;
                _ -> false
            end;
        _ -> false
    end;
is_en_passant(_, _) -> false.

is_en_passant_simple(
  Board, {IC1, IR1} = _I1, {IC2, _IR2} = _I2,
  {C, ?pawn} = _SrcFig, ?empty = _DstFig)
  when abs(IC2 - IC1) == 1 ->
    EnemyPawnIndex = {IC2, IR1},
    EnemyPawn = {not_color(C), ?pawn},
    case cell(Board, EnemyPawnIndex) == EnemyPawn of
        true ->
            {ok, EnemyPawnIndex, EnemyPawn};
        _ -> false
    end;
is_en_passant_simple(_, _, _, _, _) -> false.

is_promotion({_IC2, 1 = _IR2} = _I2, ?bpawn = _SrcFig, MoveTail) ->
    {ok, promotion_dec(MoveTail)};
is_promotion({_IC2, 8 = _IR2} = _I2, ?wpawn = _SrcFig, MoveTail) ->
    {ok, promotion_dec(MoveTail)};
is_promotion(_, _, _) -> false.

is_castling(
  {IC1, IR} = _I1, {IC2, IR} = _I2,
  {C, ?king} = _SrcFig, ?empty = _DstFig)
  when abs(IC2 - IC1) == 2
       andalso
       ((C == ?black andalso IR == 8 andalso IC1 == 5)
        orelse
          (C == ?white andalso IR == 1 andalso IC1 == 5)) ->
    case {C, IC2 - IC1} of
        {?black, -2} -> {ok, "a8d8"};
        {?black, 2}  -> {ok, "h8f8"};
        {?white, -2} -> {ok, "a1d1"};
        {?white, 2}  -> {ok, "h1f1"}
    end;
is_castling(_, _, _, _) -> false.

is_empty_or_enemy(Board, MyColor, Coord) ->
    case cell(Board, Coord) of
        ?empty -> Coord;
        {Color, _} when Color /= MyColor -> Coord;
        _ -> ?null
    end.

free_cells_until_enemy(Board, MyColor, Start, Step) ->
    case crd_inc(Start, Step) of
        ?null -> [];
        Crd ->
            case cell(Board, Crd) of
                ?empty ->
                    [Crd |
                     free_cells_until_enemy(
                       Board, MyColor, Crd, Step)];
                {MyColor, _} -> [];
                _ -> [Crd]
            end
    end.

knight_steps() ->
    [{1, 2}, {-1, 2}, {1, -2}, {-1, -2},
     {2, 1}, {-2, 1}, {2, -1}, {-2, -1}].

is_cell_under_attack(Board, I, Color) ->
    EnemyColor = not_color(Color),
    lists:any(
      fun(Step) ->
              case cell(Board, crd_inc(I, Step)) of
                  {EnemyColor, ?knight} -> true;
                  _ -> false
              end
      end, knight_steps())
        orelse
        lists:any(
          fun({DC, DR} = Step) ->
                  case find_enemy(
                         Board, I, Step, EnemyColor) of
                      {?pawn, 1}
                        when EnemyColor == ?black
                             andalso abs(DC) == 1
                             andalso DR == 1 ->
                          true;
                      {?pawn, 1}
                        when EnemyColor == ?white
                             andalso abs(DC) == 1
                             andalso DR == -1 ->
                          true;
                      {?king, 1} ->
                          true;
                      {?queen, _} ->
                          true;
                      {?bishop, _}
                        when abs(DC) == abs(DR) ->
                          true;
                      {?rook, _}
                        when abs(DC) /= abs(DR) ->
                          true;
                      _ -> false
                  end
          end,
          [{-1, -1}, {-1, 0}, {-1, 1}, {0, -1},
           {0, 1}, {1, -1}, {1, 0}, {1, 1}]).

find_enemy(Board, I, Step, EnemyColor) ->
    find_enemy(Board, I, Step, EnemyColor, 1).
find_enemy(Board, I, Step, EnemyColor, Distance) ->
    I2 = crd_inc(I, Step),
    case cell(Board, I2) of
        {EnemyColor, ChessmanType} ->
            {ChessmanType, Distance};
        ?empty ->
            find_enemy(
              Board, I2, Step,
              EnemyColor, Distance + 1);
        _ ->
            %% board end reached or
            %% friendly chessman found
            undefined
    end.

is_king_have_been_moved(History, ?white) ->
    lists:any(fun(Move) -> lists:prefix("e1", Move) end, History);
is_king_have_been_moved(History, ?black) ->
    lists:any(fun(Move) -> lists:prefix("e8", Move) end, History).

is_rook_have_been_moved(History, C) ->
    lists:any(fun(Move) -> lists:prefix(crd_enc(C), Move) end, History).

whereis_my_king(History, ?white) ->
    whereis_my_king(History, "e1");
whereis_my_king(History, ?black) ->
    whereis_my_king(History, "e8");
whereis_my_king([[A, B, C, D] | Tail], [A, B]) ->
    whereis_my_king(Tail, [C, D]);
whereis_my_king([_ | Tail], Pos) ->
    whereis_my_king(Tail, Pos);
whereis_my_king(_, [A, B]) ->
    crd_dec(A, B).

cell(Board, {C, R})
  when R >= 1 andalso R =< 8 andalso
       C >= 1 andalso C =< 8 ->
    element(C, element(9 - R, Board));
cell(_, _) -> ?null.

setcell(Board, {C, R}, Figure) ->
    Row = element(9 - R, Board),
    NewRow = setelement(C, Row, Figure),
    setelement(9 - R, Board, NewRow).

move_dec([A, B, C, D | Tail])
  when is_integer(A), is_integer(B),
       is_integer(C), is_integer(D) ->
    case {crd_dec(A, B), crd_dec(C, D)} of
        {{_,_} = C1, {_,_} = C2} ->
            {C1, C2, Tail};
        _ ->
            throw({error, badmove})
    end;
move_dec(_) ->
    throw({error, badmove}).

crd_dec([C, R]) ->
    crd_dec(C, R).
crd_dec(C, R) ->
    Crd = {C - $a + 1, R - $1 + 1},
    case crd_ok(Crd) of
        true -> Crd;
        _ -> ?null
    end.

crd_enc({C, R}) ->
    [$a + C - 1, $1 + R - 1].

crd_inc({C, R}, {CS, RS}) ->
    Crd = {C + CS, R + RS},
    case crd_ok(Crd) of
        true -> Crd;
        _ -> ?null
    end.

crd_row({_, R}) -> R.

crd_ok({C, R})
  when C >= 1 andalso C =< 8 andalso
       R >= 1 andalso R =< 8 ->
    true;
crd_ok(_) -> false.

not_color(Color) ->
    hd([?white, ?black] -- [Color]).

promotion_dec([$r | _]) -> ?rook;
promotion_dec([$k | _]) -> ?knight;
promotion_dec([$b | _]) -> ?bishop;
promotion_dec([$q | _]) -> ?queen;
promotion_dec([_ | _] = Str) ->
    throw({error, {bad_promotion_type, Str}});
promotion_dec(_) ->
    throw({error, no_promotion_type_specified}).

