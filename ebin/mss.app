{application, 'mss', [
	{description, "MS Storage Server"},
	{vsn, "0.1.0"},
	{modules, ['fetch_blob','generic_handler','mss_app','mss_sup','store_blob','store_handler','util']},
	{registered, [mss_sup]},
	{applications, [kernel,stdlib,cowboy,lager]},
	{mod, {mss_app, []}},
	{env, []}
]}.