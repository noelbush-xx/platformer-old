{application, platformer,
 [{description, "platformer"},
  {vsn, "0.1"},
  {modules, [
    platformer,
    platformer_app,
    platformer_sup,
    platformer_deps,
    platformer_resource
  ]},
  {registered, []},
  {mod, {platformer_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
