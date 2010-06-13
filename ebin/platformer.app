%%-*- mode: erlang -*-
{application, platformer,
 [
  {description, "platformer"},
  {vsn, "1"},
  {modules, [
             %% Basic modules
             platformer,
             platformer_app,
             platformer_sup,
             platformer_db,
             platformer_error_handler,
             %% Webmachine Resources
             ping_resource,
             user_resource,
             server_resource,
             %% Utility modules
             liaison,
             pfr,
             json,
             util,
             %% Externals
             crone,
             jsonerl,
             timed_supervisor,
             uuid
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
