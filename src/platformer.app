{application, platformer,
 [{description, "Platformer node"},
  {vsn, "0.1"},
  {modules, [
    platformer,
    platformer_app,
    platformer_sup,
    platformer_deps,
    platformer_error_handler,
    platformer_resource,
    wmtrace_resource
  ]},
  {registered, []},
  {mod, {platformer_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
