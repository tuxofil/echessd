%%%-------------------------------------------------------------------
%%% File    : echessd.app
%%% Author  : Aleksey Morarash <aleksey.morarash@gmail.com>
%%% Created : 20 Jan 2012
%%% License : FreeBSD
%%% Description : application resource file
%%%
%%%-------------------------------------------------------------------

{application, echessd,
 [
  {description,  "Online chess server"},
  {vsn,          "0.1.0"},
  {modules,      [echessd,
                  echessd_app,
                  echessd_sup,
                  echessd_lib,
                  echessd_log,
                  echessd_cfg,
                  echessd_srv,
                  echessd_user,
                  echessd_game,
                  echessd_db,
                  echessd_session,
                  echessd_html,
                  echessd_rules_classic
                 ]},
  {registered,   [echessd_sup, echessd_log, echessd_srv]},
  {applications, [mnesia]},
  {env,          []},
  {mod,          {echessd_app, []}}
 ]}.

