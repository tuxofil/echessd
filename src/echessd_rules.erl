-module(echessd_rules).

-export([is_valid_move/4]).

-include("echessd.hrl").

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

is_valid_move_(?GAME_CLASSIC, Game, TurnColor, Move) ->
    {C1, C2} =
        case Move of
            [A, B, C, D] -> {[A, B], [C, D]};
            _ -> throw({error, badmove})
        end,
    Figure =
        case echessd_game:getcell(Game, C1) of
            ?empty -> throw({error, {cell_is_empty, C1}});
            {TurnColor, Figure0} -> Figure0;
            {_, _} -> throw({error, not_your_figure})
        end,
    case echessd_game:getcell(Game, C2) of
        {TurnColor, _} -> throw({error, friendly_fire});
        {_, ?king} -> throw({error, cannot_take_king});
        _ -> ok
    end,
    ok;
is_valid_move_(_, _, _, _) ->
    throw(game_type_unsupported).

