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
    Figure =
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
    ok = classic_move(Table, Figure, C1, C2),
    %% todo: check post-cases
    ok;
is_valid_move_(_, _, _, _) ->
    throw(game_type_unsupported).

classic_move(Table, {Color, ?pawn}, From, To) ->
    {StartRow, Direction} =
        if Color == ?white -> {2, 1};
           true -> {7, -1}
        end,
    F1 = crd_inc(From, {0, Direction}),
    F2 = crd_inc(From, {0, Direction * 2}),
    FL = crd_inc(From, {-1, Direction}),
    FR = crd_inc(From, {1, Direction}),
    Possible =
        case cell(Table, F1) of
            ?empty ->
                [F1] ++
                    case crd_row(From) of
                        StartRow ->
                            case cell(Table, F2) of
                                ?empty -> [F2];
                                _ -> []
                            end;
                        _ -> []
                    end;
            _ -> []
        end ++
        case cell(Table, FL) of
            {_, _} -> [FL];
            _ -> []
        end ++
        case cell(Table, FR) of
            {_, _} -> [FR];
            _ -> []
        end,
    case lists:member(To, Possible) of
        true -> ok;
        _ ->
            throw({error, pawn_cannot_do_that})
    end;
classic_move(_Table, {_Color, ?knight}, From, To) ->
    Possible =
        [crd_inc(From, {1, 2}),
         crd_inc(From, {-1, 2}),
         crd_inc(From, {1, -2}),
         crd_inc(From, {-1, -2}),
         crd_inc(From, {2, 1}),
         crd_inc(From, {-2, 1}),
         crd_inc(From, {2, -1}),
         crd_inc(From, {-2, -1})
        ],
    case lists:member(To, Possible) of
        true -> ok;
        _ ->
            throw({error, knight_cannot_do_that})
    end;
classic_move(Game, {Color, Figure}, From, To) ->
    ok.

%% ----------------------------------------------------------------------
%% low level tools
%% ----------------------------------------------------------------------

same_col({C, _}, {C, _}) -> true;
same_col(_, _) -> false.

same_row({_, R}, {_, R}) -> true;
same_row(_, _) -> false.

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

crd_inc({C, R}, {CS, RS}) ->
    Crd = {C + CS, R + RS},
    case crd_ok(Crd) of
        true -> Crd;
        _ -> ?null
    end.

crd_row({_, R}) -> R.
crd_col({C, _}) -> C.

crd_ok({C, R})
  when C >= 1 andalso C =< 8 andalso
       R >= 1 andalso R =< 8 ->
    true;
crd_ok(_) -> false.

