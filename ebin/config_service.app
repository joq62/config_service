%% This is the application resource file (.app file) for the 'base'
%% application.
{application, config_service,
[{description, "config_service" },
{vsn, "0.0.1" },
{modules, 
	  [config_service_app,config_service_sup,config_service,
		config]},
{registered,[config_service]},
{applications, [kernel,stdlib]},
{mod, {config_service_app,[]}},
{start_phases, []}
]}.
