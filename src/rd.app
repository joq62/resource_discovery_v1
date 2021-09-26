%% This is the application resource file (.app file) for the 'base'
%% application.
{application, rd,
[{description, "Resource discovery" },
{vsn, "0.1.0" },
{modules, 
	  [rd,rd_sup,rd_server]},
{registered,[rd]},
{applications, [kernel,stdlib]},
{mod, {rd,[]}},
{start_phases, []},
{git_path,"https://github.com/joq62/rd.git"},
{env,[{connect_nodes,['c0@c0','c2@c2',
	              'joq62-X550CA@joq62-X550CA',
		      'test@joq62-X550CA']}]}
]}.
