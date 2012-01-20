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
                  echessd_game,
                  echessd_db,
                  echessd_html
                 ]},
  {registered,   [echessd_sup, echessd_log, echessd_srv]},
  {applications, []},
  {env,          []},
  {mod,          {echessd_app, []}}
 ]}.

