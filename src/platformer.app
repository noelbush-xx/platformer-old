%%-*- mode: erlang -*-
{application, platformer,
 [
  {description, "platformer"},
  {vsn, "1"},
  {modules, [
             %% OTP- and Webmachine- related setup modules
             platformer,
             platformer.otp.app,
             platformer.otp.sup,
             %% Webmachine resources
             platformer.webmachine.ping_resource,
             platformer.webmachine.node_resource,
             platformer.webmachine.memo_resource,
             platformer.webmachine.common,
             platformer.webmachine.error_handler,
             %% Core application modules
             platformer.core.db,
             platformer.core.memo,
             platformer.core.node,
             platformer.core.user,
             platformer.core.liaison,
             platformer.core.util,
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
  {mod, {platformer.otp.app, []}},
  {env, []}
 ]}.
