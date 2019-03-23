{application, monitor,
 [{description, " monitor"},
  {vsn, "1.0.0"},
  {modules, [monitor_app,
             monitor_sup,
	     monitor_lib,
	     monitor]},
  {registered, [monitor]},
  {applications, [kernel, stdlib]},
  {mod, {monitor_app, []}}
 ]}.
