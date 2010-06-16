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
             %% Webmachine resources
             ping_resource,
             node_resource,
             user_resource,
             %% Other application modules
             pfnode,
             pfuser,
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
