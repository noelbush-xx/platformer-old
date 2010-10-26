diff -r ba821ed12c56 src/webmachine_resource.erl
--- a/src/webmachine_resource.erl	Wed Jul 14 11:02:44 2010 -0700
+++ b/src/webmachine_resource.erl	Sun Aug 22 08:07:32 2010 +0200
@@ -102,21 +102,30 @@
     no_default.
           
 wrap(Mod, Args) ->
+    Exports = module_exports(Mod),
     case Mod:init(Args) of
 	{ok, ModState} ->
 	    {ok, webmachine_resource:new(Mod, ModState, 
-                           dict:from_list(Mod:module_info(exports)), false)};
+                           dict:from_list(Exports), false)};
         {{trace, Dir}, ModState} ->
             {ok, File} = open_log_file(Dir, Mod),
             log_decision(File, v3b14),
             log_call(File, attempt, Mod, init, Args),
             log_call(File, result, Mod, init, {{trace, Dir}, ModState}),
             {ok, webmachine_resource:new(Mod, ModState,
-			dict:from_list(Mod:module_info(exports)), File)};
+			dict:from_list(Exports), File)};
 	_ ->
 	    {stop, bad_init_arg}
     end.
 
+module_exports(Mod) ->
+    lists:umerge([lists:sort(M:module_info(exports)) || M <- [Mod|module_ancestors(Mod)]]).
+
+module_ancestors([]) -> [];
+module_ancestors(Mod) ->
+    Parents = proplists:get_value(extends, Mod:module_info(attributes), []),
+    lists:flatten([[module_ancestors(M) || M <- Parents]|Parents]).
+
 do(Fun, ReqProps) when is_atom(Fun) andalso is_list(ReqProps) ->
     RState0 = proplists:get_value(reqstate, ReqProps),
     put(tmp_reqstate, empty),
