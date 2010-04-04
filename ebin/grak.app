{application, grackle,
 [{description, "An Erlang package manager"},
  {vsn, "0.1.0"},
  {modules, [ grak,
			  grak_api,
			  grak_setup,
              grak_util
  ]},
  {registered, []},
  {applications, [kernel, stdlib, sasl]}
]}.
