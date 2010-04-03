-module(grak_util).
-compile(export_all).
-include("grak.hrl").

-define(GRACKLE_DIR, ".grackle").
-define(CONFIG_NAME, ".grakconfig").

home_dir() ->
    case init:get_argument(home) of
		{ok, [[H]]} -> H;
		_ -> ?EXIT("could not access home dir", [])
	end.

config_file() ->
    ConfigPath = filename:join([home_dir(), ?CONFIG_NAME]),
    case file:consult(ConfigPath) of
        {ok, Props} -> 
            Props;
        {error, enoent} ->
            Defaults = defaults(),
            {ok, FD} = file:open(ConfigPath, [write]),
            [io:format(FD, "~p.~n", [Tuple]) || Tuple <- Defaults],
            file:close(FD),
            Defaults;
        Error -> 
            ?EXIT("failed to read .grakconfig file: ~p", [Error])
    end.

defaults() ->
    [{build_dir, filename:join([home_dir(), ?GRACKLE_DIR, "tmp"])},
     {install_dir, filename:join([home_dir(), ?GRACKLE_DIR, "lib"])},
     {spec_dir, filename:join([home_dir(), ?GRACKLE_DIR, "specs"])}].
