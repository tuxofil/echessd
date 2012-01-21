-ifndef(_ECHESSD).
-define(_ECHESSD, true).

%% Configuration storage (ETS) name
-define(echessd_cfg, echessd_cfg).

%% Configuration items names
-define(CFG_LOGLEVEL, loglevel).
-define(CFG_LOGFILE, logfile).
-define(CFG_BIND_ADDR, bind_addr).
-define(CFG_BIND_PORT, bind_port).

-define(CFGS, [?CFG_LOGLEVEL, ?CFG_LOGFILE,
               ?CFG_BIND_ADDR, ?CFG_BIND_PORT]).

-define(MANDATORY_CFGS, []).

%% database tables
-define(dbt_users, echessd_dbt_users).
-define(dbt_games, echessd_dbt_games).

%% log message classes
-define(LOG_ERR, err).
-define(LOG_INFO, info).
-define(LOG_DEBUG, debug).

-define(LOG_LEVELS, [?LOG_ERR, ?LOG_INFO, ?LOG_DEBUG]).

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

-endif.

