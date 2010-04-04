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

del_dir(Dir) ->
	case file:list_dir(Dir) of
		{ok, Files} ->
			[begin
				case file:delete(filename:join([Dir, Filename])) of
					ok -> ok;
					{error, eperm} ->
						case file:del_dir(filename:join([Dir, Filename])) of
							ok -> ok;
							{error, eexist} ->
								del_dir(filename:join([Dir, Filename]))
						end
				end
			end || Filename <- Files],
			file:del_dir(Dir);
		_ ->
			ok
	end.

set_cwd(Dir) ->
	case file:set_cwd(Dir) of
		ok -> 
			ok;
		Error ->
			?EXIT("failed to change working directory: ~p (~s)", [Error, Dir])
	end.

do_cmd(Cmd, fail) ->
	case do_cmd(Cmd) of
		{0, ""} ->
			ok;
		{0, Output} ->
			print_cmd_output("~s~n", [Output]);
		{_, Output} ->
			exit(Output)
	end.

do_cmd(Cmd) ->
    Results = string:tokens(os:cmd(Cmd ++ "; echo $?"), "\n"),
    [ExitCode|Other] = lists:reverse(Results),
    {list_to_integer(ExitCode), string:join(lists:reverse(Other), "\n")}.

print_cmd_output(Format, Args) ->
	case get(verbose) of
		undefined -> print_cmd_output(Format, Args, false);
		Verbose -> print_cmd_output(Format, Args, Verbose)
	end.
	
print_cmd_output(_, _, false) -> ok; %% do not print verbose output
print_cmd_output(Format, Args, true) ->
	Str = lists:flatten(io_lib:format("    " ++ Format, Args)),
	Output0 = re:replace(Str, "\n", "\n    ", [global, {return, list}]),
	Output = re:replace(Output0, "\~", "", [global, {return, list}]),
	io:format(string:substr(Output, 1, length(Output)-4), []).
	
hash_to_uint(Key) -> 
    <<Int:128/unsigned-integer>> = erlang:md5(Key), Int.