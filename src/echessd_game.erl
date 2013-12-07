%%% @doc
%%% Game management tools.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 21 Jan 2012
%%% @copyright 2012, Aleksey Morarash

-module(echessd_game).

-export(
   [add/5,
    ack/2,
    deny/2,
    ply/3,
    move/3,
    hint/2,
    give_up/2,
    request_draw/2,
    getprops/1,
    who_must_turn/1,
    turn_color/1,
    turn_color_by_history/1,
    from_scratch/2,
    can_move/2,
    transpose/2,
    get_creator/1,
    get_watchers/1,
    get_players/1,
    get_opponent/2,
    get_player_color/2
   ]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% Type definitions
%% ----------------------------------------------------------------------

-export_type(
   [board/0,
    gametype/0,
    info/0,
    info_item/0,
    color/0,
    ply/0,
    ply_coords/0,
    history/0,
    id/0,
    chessman_type/0,
    chessman/0,
    entry/0,
    status/0,
    final_status/0
   ]).

-type gametype() :: ?GAME_CLASSIC.
%% Game type format definition.

-type board() :: tuple().
%% Game board format definition. Commonly, this format is game-type
%% specific and must be treated as opaque value outside of
%% 'echessd_rules_*' modules.

-type info() :: [info_item()].

-type info_item() ::
        {?gi_type, gametype()} |
        {?gi_moves, history()} |
        {?gi_time, erlang:timestamp()} |
        {?gi_private, boolean()} |
        {?gi_creator, echessd_user:name()} |
        {?gi_users, [{echessd_user:name(), color()}]} |
        {?gi_status, status()} |
        {?gi_winner, echessd_user:name()} |
        {?gi_winner_color, color()} |
        {?gi_draw_request_from, echessd_user:name()} |
        {?gi_acknowledged, boolean()}.

-type status() ::
        ?gs_alive | ?gs_checkmate | ?gs_draw_stalemate.

-type final_status() ::
        status() | ?gs_give_up | ?gs_draw_agreement.

-type color() :: ?white | ?black.
%% Color of the player. There is no place for latinos, amigo ;)

-type ply() :: {ply_coords(), ply_info()}.
%% String describing half-move (ply).
%%      Examples: "e2e4", "b7c6".

-type ply_coords() :: nonempty_string().

-type ply_info() :: [ply_info_item()].

-type ply_info_item() ::
        {?pi_time, GregorianSeconds :: non_neg_integer()} |
        {?pi_notation, ChessNotation :: nonempty_string()} |
        {?pi_comment, Comment :: string()}.

-type history() :: [ply()].
%% Ply sequence from the first to the last.

-type id() :: pos_integer().
%% Game unique identifier.

-type chessman_type() ::
        ?pawn | ?rook | ?knight | ?bishop | ?queen | ?king.
%% Chessman type format definition.

-type chessman() :: {color(), chessman_type()}.
%% Chessman format definition.

-type entry() :: chessman() | ?empty.

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Create new game and store it in the database.
%% Return an ID of the created game in case of success.
-spec add(GameType :: gametype(), Owner :: echessd_user:name(),
          OwnerColor :: color(), Opponent :: echessd_user:name(),
          OtherProps :: list()) ->
                 {ok, ID :: id()} | {error, Reason :: any()}.
add(GameType, Owner, OwnerColor, Opponent, OtherProps) ->
    Props =
        [{?gi_type, GameType},
         {?gi_time, now()},
         {?gi_creator, Owner},
         {?gi_status, ?gs_alive},
         {?gi_acknowledged, Owner == Opponent},
         {?gi_users,
          [{Owner, OwnerColor},
           {Opponent, hd([?black, ?white] -- [OwnerColor])}]}
         | OtherProps],
    case echessd_db:addgame(Props) of
        {ok, ID} = Ok ->
            echessd_log:info(
              "game ~9999p created: ~9999p",
              [ID, Props]),
            ok = echessd_notify:game_add(ID),
            Ok;
        {error, Reason} = Error ->
            echessd_log:err(
              "game creation failed: ~9999p (props=~99999p)",
              [Reason, Props]),
            Error
    end.

%% @doc Acknowledge the game.
-spec ack(ID :: id(), Username :: echessd_user:name()) ->
                 ok | {error, Reason :: any()}.
ack(ID, Username) ->
    case echessd_db:game_ack(ID, Username) of
        ok ->
            echessd_log:info(
              "game ~9999p confirmed by ~9999p",
              [ID, Username]),
            ok = echessd_notify:game_ack(ID),
            ok;
        {error, Reason} = Error ->
            echessd_log:err(
              "game ~9999p confirmation (by ~9999p) failed: ~9999p",
              [ID, Username, Reason]),
            Error
    end.

%% @doc Deny not confirmed game.
-spec deny(ID :: id(), Username :: echessd_user:name()) ->
                  ok | {error, Reason :: any()}.
deny(ID, Username) ->
    case echessd_db:game_deny(ID, Username) of
        ok ->
            echessd_log:info(
              "game ~9999p denied by ~9999p",
              [ID, Username]),
            ok;
        {error, Reason} = Error ->
            echessd_log:err(
              "game ~9999p deny (by ~9999p) failed: ~9999p",
              [ID, Username, Reason]),
            Error
    end.

%% @doc Try to save user ply to the database. The ply will be
%% checked for correctness.
-spec ply(ID :: id(), User :: echessd_user:name(), Ply :: ply()) ->
                 ok | {error, Reason :: any()}.
ply(ID, User, Ply) ->
    Seconds =
        calendar:datetime_to_gregorian_seconds(
          calendar:universal_time()),
    {Coords, _} = Ply1 =
        case Ply of
            {Coords0, Meta0} ->
                {Coords0,
                 [{?pi_time, Seconds} |
                  [I || {K, _} = I <- Meta0, K /= ?pi_time]]};
            _ ->
                {Ply, [{?pi_time, Seconds}]}
        end,
    case echessd_db:gameply(ID, User, Ply1) of
        {ok, Info} ->
            echessd_log:info(
              "game ~9999p: user ~9999p moved ~9999p",
              [ID, User, Coords]),
            [LastPly | _] =
                lists:reverse(proplists:get_value(?gi_moves, Info)),
            case proplists:get_value(?gi_status, Info) of
                ?gs_alive -> nop;
                _GameEndedStatus ->
                    ok = echessd_notify:game_end(ID)
            end,
            echessd_notify:game_move(ID, User, LastPly),
            ok;
        {error, Reason} = Error ->
            echessd_log:err(
              "game ~9999p: user ~9999p failed to move ~9999p: ~99999p",
              [ID, User, Coords, Reason]),
            Error
    end.

%% @doc Apply user's ply. The function is called only from
%% 'echessd_db:gameply/3' fun.
%% @hidden
-spec move(GameType :: gametype(), History :: history(), Ply :: ply()) ->
                  {ok, NewBoard :: board(),
                   NewHistory :: history(),
                   GameStatus :: final_status()}.
move(?GAME_CLASSIC, History, Ply) ->
    echessd_rules_classic:move(History, Ply).

%% @doc Return a list of all available moves.
%% The function used only for current move highlighting in
%% the user interface.
-spec hint(GameType :: gametype(), History :: history()) ->
                  Plies :: [ply_coords()].
hint(?GAME_CLASSIC, History) ->
    echessd_rules_classic:hint(History).

%% @doc Make user fail the game by giving up.
-spec give_up(ID :: id(), Username :: echessd_user:name()) ->
                     ok | {error, Reason :: any()}.
give_up(ID, Username) ->
    case echessd_db:game_give_up(ID, Username) of
        ok ->
            echessd_log:info(
              "game ~9999p gived up by ~9999p",
              [ID, Username]),
            ok = echessd_notify:game_end(ID),
            ok;
        {error, Reason} = Error ->
            echessd_log:err(
              "game ~9999p give up (by ~9999p) failed: ~9999p",
              [ID, Username, Reason]),
            Error
    end.

%% @doc Make request for a draw by agreement.
-spec request_draw(ID :: id(), Username :: echessd_user:name()) ->
                          ok | {error, Reason :: any()}.
request_draw(ID, Username) ->
    case echessd_db:game_request_draw(ID, Username) of
        ok ->
            echessd_log:info(
              "game ~9999p requested draw by ~9999p",
              [ID, Username]),
            ok;
        {error, Reason} = Error ->
            echessd_log:err(
              "game ~9999p draw request (by ~9999p) failed: ~9999p",
              [ID, Username, Reason]),
            Error
    end.

%% @doc Fetch the game properties from the database.
-spec getprops(ID :: id()) ->
                      {ok, Info :: info()} |
                      {error, Reason :: any()}.
getprops(ID) ->
    echessd_db:get_game_props(ID).

%% @doc Return user name who must make the next turn.
-spec who_must_turn(Info :: info()) ->
                           Username :: echessd_user:name().
who_must_turn(Info) ->
    Users =
        [{C, N} || {?gi_users, L} <- Info,
                   {N, C} <- L, lists:member(C, [?black, ?white])],
    proplists:get_value(turn_color(Info), Users).

%% @doc Return the color of the side that must make the next turn.
-spec turn_color(Info :: info()) -> Color :: color().
turn_color(Info) ->
    turn_color_by_history(
      proplists:get_value(?gi_moves, Info, [])).

%% @doc Return the color of the side that must make the next turn.
-spec turn_color_by_history(History :: history()) -> Color :: color().
turn_color_by_history(History) ->
    lists:nth((length(History) rem 2) + 1, [?white, ?black]).

%% @doc Create a new board and make all moves from the given history.
%% Return the chess board and a list of all chessmans captured.
-spec from_scratch(GameType :: gametype(), History :: history()) ->
                          {Board :: board(), Captures :: [chessman()]}.
from_scratch(GameType, History) ->
    lists:foldl(
      fun(Ply, {Board, Captures}) ->
              {NewGame, Capture} = move_unsafe(GameType, Board, Ply),
              {NewGame, [Capture | Captures]}
      end, {new(GameType), []}, History).

%% @doc Check if valid turn is present for the color.
-spec can_move(GameType :: gametype(), History :: history()) ->
                      boolean().
can_move(?GAME_CLASSIC, History) ->
    echessd_rules_classic:can_move(History).

%% @doc Turn the board at 180 degrees.
-spec transpose(GameType :: gametype(), Board :: board()) ->
                       TransposedBoard :: board().
transpose(?GAME_CLASSIC, Board) ->
    echessd_rules_classic:transpose(Board).

%% @doc Fetch the game creator name.
-spec get_creator((ID :: id()) |
                  (Info :: info())) ->
                         Username :: echessd_user:name().
get_creator(ID) when is_integer(ID) ->
    {ok, Info} = getprops(ID),
    get_creator(Info);
get_creator(Info) when is_list(Info) ->
    proplists:get_value(?gi_creator, Info).

%% @doc Fetch watcher names for the game.
-spec get_watchers((ID :: id()) |
                   (Info :: info())) ->
                          [Username :: echessd_user:name()].
get_watchers(ID) when is_integer(ID) ->
    {ok, Info} = getprops(ID),
    get_watchers(Info);
get_watchers(Info) when is_list(Info) ->
    Users = proplists:get_value(?gi_users, Info),
    [N || {N, _Role} <- Users].

%% @doc Fetch the player names for the game.
-spec get_players((ID :: id()) |
                  (Info :: info())) ->
                         [Username :: echessd_user:name()].
get_players(ID) when is_integer(ID) ->
    {ok, Info} = getprops(ID),
    get_players(Info);
get_players(Info) when is_list(Info) ->
    Users = proplists:get_value(?gi_users, Info),
    [N || {N, Role} <- Users, lists:member(Role, [?black, ?white])].

%% @doc Fetch the opponent for the user.
-spec get_opponent((ID :: id()) |
                   (Info :: info()),
                   Username :: echessd_user:name()) ->
                          Opponent :: echessd_user:name().
get_opponent(ID, Username) when is_integer(ID) ->
    {ok, Info} = getprops(ID),
    get_opponent(Info, Username);
get_opponent(Info, Username) when is_list(Info) ->
    [Opponent | _] = get_players(Info) -- [Username],
    Opponent.

%% @doc Fetch the color of the player.
-spec get_player_color((ID :: id()) |
                       (Info :: info()),
                       Username :: echessd_user:name()) ->
                              Color :: color().
get_player_color(ID, Username) when is_integer(ID) ->
    {ok, Info} = getprops(ID),
    get_player_color(Info, Username);
get_player_color(Info, Username) when is_list(Info) ->
    Users = proplists:get_value(?gi_users, Info),
    [Color | _] = [C || {N, C} <- Users, N == Username],
    Color.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc Create a chess board with the all chessmans at their start point.
-spec new(GameType :: gametype()) -> Board :: board().
new(?GAME_CLASSIC) ->
    echessd_rules_classic:new().

%% @doc Move the chessman regarding the rules of the game.
-spec move_unsafe(GameType :: gametype(), Board :: board(),
                  Ply :: ply()) ->
                         {NewBoard :: board(),
                          Capture :: chessman() | ?empty}.
move_unsafe(?GAME_CLASSIC, Board, Ply) ->
    echessd_rules_classic:move_unsafe(Board, Ply).
