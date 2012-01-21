-ifndef(_ECHESSD).
-define(_ECHESSD, true).

%% Configuration storage (ETS) name
-define(echessd_cfg, echessd_cfg).

%% Configuration items names
-define(CFG_LOGLEVEL, loglevel).
-define(CFG_LOGFILE, logfile).
-define(CFG_BIND_ADDR, bind_addr).
-define(CFG_BIND_PORT, bind_port).
-define(CFG_DOC_ROOT, doc_root).

-define(CFGS, [?CFG_LOGLEVEL, ?CFG_LOGFILE,
               ?CFG_BIND_ADDR, ?CFG_BIND_PORT,
               ?CFG_DOC_ROOT]).

-define(MANDATORY_CFGS, []).

%% database tables
-define(dbt_users, echessd_dbt_users).
-define(dbt_games, echessd_dbt_games).
-define(dbt_session, echessd_dbt_session).

%% log message classes
-define(LOG_ERR, err).
-define(LOG_INFO, info).
-define(LOG_DEBUG, debug).

-define(LOG_LEVELS, [?LOG_ERR, ?LOG_INFO, ?LOG_DEBUG]).

%% game styles
-define(GAME_CLASSIC, classic).

-define(GAME_STYLES, [?GAME_CLASSIC]).

%% site sections
-define(SECTION_HOME, "home").
-define(SECTION_GAME, "game").
-define(SECTION_TEST, "test").
-define(SECTION_USERS, "users").

-define(SECTIONS, [?SECTION_HOME, ?SECTION_GAME, ?SECTION_TEST,
                   ?SECTION_USERS]).

%% colors
-define(white, w).
-define(black, b).

%% figures
-define(pawn, p).
-define(rook, r).
-define(knight, h).
-define(bishop, b).
-define(queen, q).
-define(king, k).

-define(w_pawn, {?white, ?pawn}).
-define(b_pawn, {?black, ?pawn}).
-define(w_rook, {?white, ?rook}).
-define(b_rook, {?black, ?rook}).
-define(w_knight, {?white, ?knight}).
-define(b_knight, {?black, ?knight}).
-define(w_bishop, {?white, ?bishop}).
-define(b_bishop, {?black, ?bishop}).
-define(w_queen, {?white, ?queen}).
-define(b_queen, {?black, ?queen}).
-define(w_king, {?white, ?king}).
-define(b_king, {?black, ?king}).

-define(empty, z).

%% ----------------------------------------------------------------------

-define(nonnegint(I), (is_integer(I) andalso I >= 0)).

-define(is_now(T),
        (is_tuple(T)
         andalso size(T) == 3
         andalso ?nonnegint(element(1, T))
         andalso ?nonnegint(element(2, T))
         andalso ?nonnegint(element(3, T)))).

-endif.

