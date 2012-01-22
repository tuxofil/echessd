-module(echessd_rules).

-export([is_valid_move/4]).

-include("echessd.hrl").

-define(null, '*null').

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

is_valid_move(GameType, Game, TurnColor, Move) ->
    try is_valid_move_(GameType, Game, TurnColor, Move) of
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

is_valid_move_(?GAME_CLASSIC, Table, TurnColor, Move) ->
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
    Possible = possible(Table, C1, MyColor, Figure),
    echessd_log:debug(
      "Fig ~w from ~s can move to: ~9999p",
      [{MyColor, Figure}, crd_enc(C1),
       [crd_enc(I) || I <- Possible]]),
    case lists:member(C2, Possible) of
        true ->
            ok;
        _ ->
            throw({error, badmove})
    end;
is_valid_move_(_, _, _, _) ->
    throw(game_type_unsupported).

possible(T, F, C, Fig) ->
    lists:usort(lists:flatten(possible_(T, F, C, Fig))) -- [?null].
possible_(T, F, C, ?pawn) ->
    {StartRow, Direction} =
        if C == ?white -> {2, 1};
           true -> {7, -1}
        end,
    F1 = crd_inc(F, {0, Direction}),
    F2 = crd_inc(F, {0, Direction * 2}),
    FL = crd_inc(F, {-1, Direction}),
    FR = crd_inc(F, {1, Direction}),
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
        end;
possible_(T, F, C, ?rook) ->
    [nexts(T, C, F, {0, 1}),
     nexts(T, C, F, {0, -1}),
     nexts(T, C, F, {1, 0}),
     nexts(T, C, F, {-1, 0})];
possible_(T, F, C, ?bishop) ->
    [nexts(T, C, F, {-1, -1}),
     nexts(T, C, F, {1, -1}),
     nexts(T, C, F, {-1, 1}),
     nexts(T, C, F, {1, 1})];
possible_(T, F, C, ?queen) ->
    [possible_(T, F, C, ?bishop),
     possible_(T, F, C, ?rook)];
possible_(T, F, C, ?knight) ->
    [is_empty_or_enemy(T, C, crd_inc(F, {1, 2})),
     is_empty_or_enemy(T, C, crd_inc(F, {-1, 2})),
     is_empty_or_enemy(T, C, crd_inc(F, {1, -2})),
     is_empty_or_enemy(T, C, crd_inc(F, {-1, -2})),
     is_empty_or_enemy(T, C, crd_inc(F, {2, 1})),
     is_empty_or_enemy(T, C, crd_inc(F, {-2, 1})),
     is_empty_or_enemy(T, C, crd_inc(F, {2, -1})),
     is_empty_or_enemy(T, C, crd_inc(F, {-2, -1}))];
possible_(T, F, C, ?king) ->
    [is_empty_or_enemy(T, C, crd_inc(F, {-1, -1})),
     is_empty_or_enemy(T, C, crd_inc(F, {-1, 0})),
     is_empty_or_enemy(T, C, crd_inc(F, {-1, 1})),
     is_empty_or_enemy(T, C, crd_inc(F, {0, -1})),
     is_empty_or_enemy(T, C, crd_inc(F, {0, 1})),
     is_empty_or_enemy(T, C, crd_inc(F, {1, -1})),
     is_empty_or_enemy(T, C, crd_inc(F, {1, 0})),
     is_empty_or_enemy(T, C, crd_inc(F, {1, 1}))];
possible_(_, _, _, _) -> [].

%% ----------------------------------------------------------------------
%% low level tools
%% ----------------------------------------------------------------------

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
    echessd_log:debug("COORD ~s DECODED to ~w", [[C, R], Crd]),
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

