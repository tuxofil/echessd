%%%-------------------------------------------------------------------
%%% File    : echessd.hrl
%%% Author  : Aleksey Morarash <aleksey.morarash@gmail.com>
%%% Created : 20 Jan 2012
%%% License : FreeBSD
%%% Description : common constants definitions
%%%
%%%-------------------------------------------------------------------

-ifndef(_ECHESSD).
-define(_ECHESSD, true).

%% Configuration items names
-define(CFG_LOGLEVEL, loglevel).
-define(CFG_LOGFILE, logfile).
-define(CFG_BIND_ADDR, bind_addr).
-define(CFG_BIND_PORT, bind_port).
-define(CFG_DOC_ROOT, doc_root).
-define(CFG_LANG_INFO, lang_info).
-define(CFG_DEF_LANG, default_language).
-define(CFG_HTTPD_MOD, httpd_engine).
-define(CFG_XMPP_ENABLED, xmpp_enabled).
-define(CFG_XMPP_USER, xmpp_user).
-define(CFG_XMPP_SERVER, xmpp_server).
-define(CFG_XMPP_PASSWORD, xmpp_password).

-define(CFGS, [?CFG_LOGLEVEL, ?CFG_LOGFILE,
               ?CFG_BIND_ADDR, ?CFG_BIND_PORT,
               ?CFG_DOC_ROOT, ?CFG_DEF_LANG,
               ?CFG_HTTPD_MOD, ?CFG_XMPP_USER,
               ?CFG_XMPP_SERVER, ?CFG_XMPP_PASSWORD,
               ?CFG_XMPP_ENABLED]).

%% available HTTPD implementations
-define(HTTPD_MODULES,
        [echessd_httpd_mochiweb, echessd_httpd_inets]).

-define(MANDATORY_CFGS, []).

%% echessd node names
-define(NODE_ECHESSD, "echessd").

%% Configuration storage (ETS) name
-define(echessd_cfg, echessd_cfg).

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
-define(GAME_CLASSIC, "classic").

-define(GAME_TYPES, [?GAME_CLASSIC]).

%% site sections
-define(SECTION_HOME, "home").
-define(SECTION_GAME, "game").
-define(SECTION_USERS, "users").
-define(SECTION_USER, "user").
-define(SECTION_NEWGAME, "newgame").
-define(SECTION_REG, "register").
-define(SECTION_LOGIN, "login").
-define(SECTION_EXIT, "exit").
-define(SECTION_MOVE, "move").
-define(SECTION_ACKGAME, "ackgame").
-define(SECTION_DENYGAME, "denygame").
-define(SECTION_EDITUSER, "edituser").
-define(SECTION_SAVEUSER, "saveuser").
-define(SECTION_PASSWD_FORM, "passwdform").
-define(SECTION_PASSWD, "passwd").
-define(SECTION_DRAW_CONFIRM, "drawconfirm").
-define(SECTION_DRAW, "draw").
-define(SECTION_GIVEUP_CONFIRM, "giveupconfirm").
-define(SECTION_GIVEUP, "giveup").

-define(SECTIONS,
        [?SECTION_HOME, ?SECTION_GAME,
         ?SECTION_USERS, ?SECTION_USER,
         ?SECTION_NEWGAME,
         ?SECTION_EDITUSER, ?SECTION_DRAW_CONFIRM,
         ?SECTION_GIVEUP_CONFIRM, ?SECTION_PASSWD_FORM
        ]).

%% colors
-define(white, w).
-define(black, b).

%% chessman types
-define(pawn, p).
-define(rook, r).
-define(knight, h).
-define(bishop, b).
-define(queen, q).
-define(king, k).

%% chessmans
-define(wpawn, {?white, ?pawn}).
-define(bpawn, {?black, ?pawn}).
-define(wrook, {?white, ?rook}).
-define(brook, {?black, ?rook}).
-define(wknight, {?white, ?knight}).
-define(bknight, {?black, ?knight}).
-define(wbishop, {?white, ?bishop}).
-define(bbishop, {?black, ?bishop}).
-define(wqueen, {?white, ?queen}).
-define(bqueen, {?black, ?queen}).
-define(wking, {?white, ?king}).
-define(bking, {?black, ?king}).

-define(empty, z).

%% ----------------------------------------------------------------------
%% styles

-define(STYLE_DEFAULT, default).
-define(STYLE_BW, bw).
-define(STYLE_TERM, term).
-define(STYLE_OLDTERM, oldterm).

-define(
   STYLES,
   [{?STYLE_DEFAULT, txt_style_default, "default.css"},
    {?STYLE_BW, txt_style_bw, "bw.css"},
    {?STYLE_TERM, txt_style_term, "term.css"},
    {?STYLE_OLDTERM, txt_style_oldterm, "oldterm.css"}
   ]).

%% ----------------------------------------------------------------------

-define(nonnegint(I), (is_integer(I) andalso I >= 0)).

-define(is_now(T),
        (is_tuple(T)
         andalso size(T) == 3
         andalso ?nonnegint(element(1, T))
         andalso ?nonnegint(element(2, T))
         andalso ?nonnegint(element(3, T)))).

-endif.

