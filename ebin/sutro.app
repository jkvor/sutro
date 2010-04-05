{application, sutro,
 [{description, "An Erlang package manager"},
  {vsn, "0.1.0"},
  {modules, [ sutro,
			  sutro_api,
			  sutro_setup,
              sutro_util
  ]},
  {registered, []},
  {applications, [kernel, stdlib, sasl]}
]}.
