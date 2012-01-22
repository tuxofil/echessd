-module(echessd_rules).

-export([is_valid_move/5]).

-include("echessd.hrl").

-define(null, '*null').

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

is_valid_move(GameType, Game, TurnColor, Move, History) ->
    try is_valid_move_(GameType, Game, TurnColor, Move, History) of
        ok -> ok;
        {error, _} = Error -> Error;
        Other -> {error, Other}
    catch
        _:{error, _} = ExcError -> ExcError;
        Type:Reason ->
            {error, {Type, Reason, erlang:get_stacktrace()}}
    end.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

is_valid_move_(?GAME_CLASSIC, Table, TurnColor, Move, History) ->
    {C1, C2} = move_dec(Move),
    {MyColor, Figure} =
        case cell(Table, C1) of
            ?empty -> throw({error, {cell_is_empty, C1}});
            {TurnColor, _} = Figure0 -> Figure0;
            {_, _} -> throw({error, not_your_figure})
        end,
    case cell(Table, C2) of
        {TurnColor, _} -> throw({error, friendly_fire});
        {_, ?king} -> throw({error, cannot_take_king});
        _ -> ok
    end,
    Possible = possible(Table, C1, MyColor, Figure, History),
    echessd_log:debug(
      "Fig ~w from ~s can move to: ~9999p",
      [{MyColor, Figure}, crd_enc(C1),
       [crd_enc(I) || I <- Possible]]),
    case lists:member(C2, Possible) of
        true ->
            {Table2, _Took} =
                echessd_game:ll_move(
                  Table, crd_enc(C1), crd_enc(C2)),
            MyKingIndex =
                if Figure == ?king -> C2;
                   true ->
                        whereis_my_king(History, MyColor)
                end,
            %% fixme: need to use more efficient way
            Enemies = all_enemies(Table2, MyColor),
            case is_cell_under_attack(
                   Table2, MyKingIndex, Enemies) of
                true ->
                    throw({error, check});
                _ -> ok
            end;
        _ ->
            throw({error, badmove})
    end;
is_valid_move_(_, _, _, _, _) ->
    throw(game_type_unsupported).

possible(T, F, C, Fig, History) ->
    lists:usort(
      lists:flatten(
        possible_(T, F, C, Fig, History))) -- [?null].
possible_(T, F, C, ?pawn, History) ->
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
    OppPawn = {hd([?black, ?white] -- [C]), ?pawn},
    case cell(T, F1) of
        ?empty ->
            [F1] ++
                case crd_row(F) of
                    StartRow ->
                        case cell(T, F2) of
                            ?empty -> [F2];
                            _ -> []
                        end;
                    _ -> []
                end;
        _ -> []
    end ++
        case cell(T, FL) of
            {_, _} -> [FL];
            _ -> []
        end ++
        case cell(T, FR) of
            {_, _} -> [FR];
            _ -> []
        end ++
        case cell(T, EnPassL) of
            OppPawn ->
                case is_en_passant(History, EnPassL) of
                    true ->
                        [crd_inc(F, {-1, Direction})];
                    _ -> []
                end;
            _ -> []
        end ++
        case cell(T, EnPassR) of
            OppPawn ->
                case is_en_passant(History, EnPassR) of
                    true ->
                        [crd_inc(F, {1, Direction})];
                    _ -> []
                end;
            _ -> []
        end;
possible_(T, F, C, ?rook, _History) ->
    [nexts(T, C, F, {0, 1}),
     nexts(T, C, F, {0, -1}),
     nexts(T, C, F, {1, 0}),
     nexts(T, C, F, {-1, 0})];
possible_(T, F, C, ?bishop, _History) ->
    [nexts(T, C, F, {-1, -1}),
     nexts(T, C, F, {1, -1}),
     nexts(T, C, F, {-1, 1}),
     nexts(T, C, F, {1, 1})];
possible_(T, F, C, ?queen, History) ->
    [possible_(T, F, C, ?bishop, History),
     possible_(T, F, C, ?rook, History)];
possible_(T, F, C, ?knight, _History) ->
    [is_empty_or_enemy(T, C, crd_inc(F, {1, 2})),
     is_empty_or_enemy(T, C, crd_inc(F, {-1, 2})),
     is_empty_or_enemy(T, C, crd_inc(F, {1, -2})),
     is_empty_or_enemy(T, C, crd_inc(F, {-1, -2})),
     is_empty_or_enemy(T, C, crd_inc(F, {2, 1})),
     is_empty_or_enemy(T, C, crd_inc(F, {-2, 1})),
     is_empty_or_enemy(T, C, crd_inc(F, {2, -1})),
     is_empty_or_enemy(T, C, crd_inc(F, {-2, -1}))];
possible_(T, F, C, ?king, _History) ->
    [is_empty_or_enemy(T, C, crd_inc(F, {-1, -1})),
     is_empty_or_enemy(T, C, crd_inc(F, {-1, 0})),
     is_empty_or_enemy(T, C, crd_inc(F, {-1, 1})),
     is_empty_or_enemy(T, C, crd_inc(F, {0, -1})),
     is_empty_or_enemy(T, C, crd_inc(F, {0, 1})),
     is_empty_or_enemy(T, C, crd_inc(F, {1, -1})),
     is_empty_or_enemy(T, C, crd_inc(F, {1, 0})),
     is_empty_or_enemy(T, C, crd_inc(F, {1, 1}))];
possible_(_, _, _, _, _) -> [].

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

is_empty_or_enemy(Table, MyColor, Coord) ->
    case cell(Table, Coord) of
        ?empty -> Coord;
        {Color, _} when Color /= MyColor -> Coord;
        _ -> ?null
    end.

nexts(Table, MyColor, Start, Step) ->
    case crd_inc(Start, Step) of
        ?null -> [];
        Crd ->
            case cell(Table, Crd) of
                ?empty ->
                    [Crd | nexts(Table, MyColor, Crd, Step)];
                {MyColor, _} -> [];
                _ -> [Crd]
            end
    end.

is_cell_under_attack(Table, Crd, Enemies) ->
    lists:any(
      fun({I, {C, F}}) ->
              lists:member(
                Crd,
                possible(Table, I, C, F, []))
      end, Enemies).

-define(seq_1_8, [1,2,3,4,5,6,7,8]).
all_enemies(Table, MyColor) ->
    lists:flatmap(
      fun(Crd) ->
              case cell(Table, Crd) of
                  {Color, _} = F when Color /= MyColor ->
                      [{Crd, F}];
                  _ -> []
              end
      end, [{C, R} || C <- ?seq_1_8, R <- ?seq_1_8]).

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

cell(Table, {C, R})
  when R >= 1 andalso R =< 8 andalso
       C >= 1 andalso C =< 8 ->
    element(C, element(9 - R, Table));
cell(_, _) -> ?null.

move_dec([A, B, C, D])
  when is_integer(A), is_integer(B),
       is_integer(C), is_integer(D) ->
    case {crd_dec(A, B), crd_dec(C, D)} of
        {{_,_}, {_,_}} = Coords ->
            Coords;
        _ ->
            throw({error, badmove})
    end;
move_dec(_) ->
    throw({error, badmove}).

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

