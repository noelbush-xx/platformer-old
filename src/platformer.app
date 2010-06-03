%%-*- mode: erlang -*-
{application, platformer,
 [
  {description, "platformer"},
  {vsn, "1"},
  {modules, [
             platformer,
             platformer_app,
             platformer_sup,
             platformer_db,
             platformer_error_handler,
             pfr,
             json,
             util,
             uuid,
             ping_resource,
             user_resource,
             server_resource
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib,
                  crypto,
                  mochiweb,
                  webmachine
                 ]},
  {mod, { platformer_app, []}},
  {env, []}
 ]}.
