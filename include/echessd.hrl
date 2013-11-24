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
-define(CFG_DEF_LANG, default_language).
-define(CFG_DEF_STYLE, default_style).
-define(CFG_XMPP_ENABLED, xmpp_enabled).
-define(CFG_XMPP_USER, xmpp_user).
-define(CFG_XMPP_SERVER, xmpp_server).
-define(CFG_XMPP_PASSWORD, xmpp_password).
-define(CFG_SHOW_ABOUT, show_about).
-define(CFG_SHOW_COPYRIGHTS, show_copyrights).
-define(CFG_MIME_TYPES, mime_types).
-define(CFG_INSTANCE_ID, instance_id).
-define(CFG_COOKIE, cookie).

-define(CFGS, [?CFG_LOGLEVEL, ?CFG_LOGFILE,
               ?CFG_BIND_ADDR, ?CFG_BIND_PORT,
               ?CFG_DEF_LANG, ?CFG_DEF_STYLE,
               ?CFG_XMPP_USER, ?CFG_XMPP_SERVER,
               ?CFG_XMPP_PASSWORD, ?CFG_XMPP_ENABLED,
               ?CFG_SHOW_ABOUT, ?CFG_SHOW_COPYRIGHTS,
               ?CFG_MIME_TYPES, ?CFG_INSTANCE_ID,
               ?CFG_COOKIE]).

-define(CFG_CONFIG_PATH, config_path).

%% log message classes
-define(LOG_ERR, err).
-define(LOG_INFO, info).
-define(LOG_DEBUG, debug).

-define(LOG_LEVELS, [?LOG_ERR, ?LOG_INFO, ?LOG_DEBUG]).

%% game types
-define(GAME_CLASSIC, classic).
-define(GAME_TYPES, [?GAME_CLASSIC]).

%% site sections
-define(SECTION_HOME, home).
-define(SECTION_GAME, game).
-define(SECTION_USERS, users).
-define(SECTION_USER, user).
-define(SECTION_NEWGAME, newgame).
-define(SECTION_REG, register).
-define(SECTION_LOGIN, login).
-define(SECTION_EXIT, exit).
-define(SECTION_MOVE, move).
-define(SECTION_ACKGAME, ackgame).
-define(SECTION_DENYGAME, denygame).
-define(SECTION_EDITUSER, edituser).
-define(SECTION_SAVEUSER, saveuser).
-define(SECTION_PASSWD_FORM, passwdform).
-define(SECTION_PASSWD, passwd).
-define(SECTION_DRAW_CONFIRM, drawconfirm).
-define(SECTION_DRAW, draw).
-define(SECTION_GIVEUP_CONFIRM, giveupconfirm).
-define(SECTION_GIVEUP, giveup).

-define(ALL_SECTIONS,
        [?SECTION_HOME, ?SECTION_GAME, ?SECTION_USERS,
         ?SECTION_USER, ?SECTION_NEWGAME, ?SECTION_REG,
         ?SECTION_LOGIN, ?SECTION_EXIT, ?SECTION_MOVE,
         ?SECTION_ACKGAME, ?SECTION_DENYGAME, ?SECTION_EDITUSER,
         ?SECTION_SAVEUSER, ?SECTION_PASSWD_FORM, ?SECTION_PASSWD,
         ?SECTION_DRAW_CONFIRM, ?SECTION_DRAW, ?SECTION_GIVEUP_CONFIRM,
         ?SECTION_GIVEUP]).

-define(SECTIONS,
        [?SECTION_HOME, ?SECTION_GAME,
         ?SECTION_USERS, ?SECTION_USER,
         ?SECTION_NEWGAME,
         ?SECTION_EDITUSER, ?SECTION_DRAW_CONFIRM,
         ?SECTION_GIVEUP_CONFIRM, ?SECTION_PASSWD_FORM
        ]).

%% http query keys
-define(Q_GOTO, goto).
-define(Q_STEP, step).
-define(Q_GAME, game).
-define(Q_NAME, name).
-define(Q_MOVE, move).
-define(Q_COMMENT, comment).
-define(Q_PRIVATE, private).
-define(Q_GAMETYPE, gametype).
-define(Q_COLOR, color).
-define(Q_OPPONENT, opponent).
-define(Q_EDIT_JID, editjid).
-define(Q_EDIT_STYLE, editstyle).
-define(Q_EDIT_AUTO_PERIOD, editautoperiod).
-define(Q_EDIT_AUTO_REFRESH, editautorefresh).
-define(Q_EDIT_NOTIFY, editnotify).
-define(Q_EDIT_SHOW_COMMENT, editshowcomment).
-define(Q_EDIT_SHOW_HISTORY, editshowhistory).
-define(Q_EDIT_SHOW_IN_LIST, editshowinlist).
-define(Q_EDIT_LANGUAGE, editlanguage).
-define(Q_EDIT_TIMEZONE, edittimezone).
-define(Q_EDIT_FULLNAME, editfullname).
-define(Q_EDIT_PASSWORD0, editpassword0).
-define(Q_EDIT_PASSWORD1, editpassword1).
-define(Q_EDIT_PASSWORD2, editpassword2).
-define(Q_EDIT_USERNAME, editusername).
-define(Q_USERNAME, username).
-define(Q_PASSWORD, password).
-define(Q_LANG, lang).
-define(Q_USER, user).

-define(ALL_Q_KEYS,
        [?Q_GOTO, ?Q_STEP, ?Q_GAME, ?Q_NAME, ?Q_MOVE, ?Q_COMMENT,
         ?Q_PRIVATE, ?Q_GAMETYPE, ?Q_COLOR, ?Q_OPPONENT, ?Q_EDIT_JID,
         ?Q_EDIT_STYLE, ?Q_EDIT_AUTO_PERIOD, ?Q_EDIT_AUTO_REFRESH,
         ?Q_EDIT_NOTIFY, ?Q_EDIT_SHOW_COMMENT, ?Q_EDIT_SHOW_HISTORY,
         ?Q_EDIT_SHOW_IN_LIST, ?Q_EDIT_LANGUAGE, ?Q_EDIT_TIMEZONE,
         ?Q_EDIT_FULLNAME, ?Q_EDIT_PASSWORD0, ?Q_EDIT_PASSWORD1,
         ?Q_EDIT_PASSWORD2, ?Q_EDIT_USERNAME, ?Q_USERNAME, ?Q_PASSWORD,
         ?Q_LANG, ?Q_USER
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

%% user session record
-record(session,
        {id :: echessd_session:id(),
         created :: erlang:timestamp(),
         username :: echessd_user:name() | undefined,
         timezone :: echessd_lib:administrative_offset() | undefined,
         language :: (LanguageID :: atom() | undefined),
         style :: (StyleID :: atom() | undefined),
         userinfo = [] :: echessd_user:info(),
         vars = [] :: [{Key :: any(), Value :: any()}]
        }).

%% ----------------------------------------------------------------------

-define(nonnegint(I), (is_integer(I) andalso I >= 0)).

-define(is_now(T),
        (is_tuple(T)
         andalso size(T) == 3
         andalso ?nonnegint(element(1, T))
         andalso ?nonnegint(element(2, T))
         andalso ?nonnegint(element(3, T)))).

%% ----------------------------------------------------------------------

-ifndef(WITHOUT_INETS_HEADER).
-include_lib("inets/include/httpd.hrl").
-else.
%% early versions of Erlang on Debian didn't provide inets/include/httpd.hrl
-record(mod,
        {init_data,
         data=[],
         socket_type=ip_comm,
         socket,
         config_db,
         method,
         absolute_uri=[],
         request_uri,
         http_version,
         request_line,
         parsed_header=[],
         entity_body,
         connection}).
-endif.

-define(HTTP_GET, "GET").
-define(HTTP_POST, "POST").

-endif.
