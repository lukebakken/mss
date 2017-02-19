{application, 'mss', [
	{description, "MS Storage Server"},
	{vsn, "0.1.0"},
	{modules, ['generic_handler','mss_app','mss_sup','store_handler']},
	{registered, [mss_sup]},
	{applications, [kernel,stdlib,cowboy,lager]},
	{mod, {mss_app, []}},
	{env, []}
]}.