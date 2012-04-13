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
  {vsn,          "0.2.0"},
  {modules,      [echessd,
                  echessd_app,
                  echessd_sup,
                  echessd_lib,
                  echessd_log,
                  echessd_cfg,
                  echessd_request_processor,
                  echessd_user,
                  echessd_game,
                  echessd_notify,
                  echessd_db,
                  echessd_session,
                  echessd_html,
                  echessd_rules_classic,
                  echessd_web_warden,
                  echessd_httpd,
                  echessd_httpd_lib,
                  echessd_httpd_inets,
                  echessd_httpd_mochiweb
                 ]},
  {registered,   [echessd_sup, echessd_log,
                  echessd_web_warden]},
  {applications, [mnesia]},
  {env,          []},
  {mod,          {echessd_app, []}}
 ]}.

