-module(sutro_api).
-export([dispatch/1]).
-include("sutro.hrl").

dispatch(["search" | Args]) ->
    {SearchStr, Opts} = collect_args(search, Args),
    Packages = sutro:search(SearchStr, Opts),
    io:format("--> found ~w packages~n", [length(Packages)]),
    [begin
        io:format("--> ~s~n", [PackageName]),
        io:format("      ~s~n", [proplists:get_value(url, Props, "")])
    end || {PackageName, Props} <- Packages];
    
dispatch(["install" | Args]) ->
    {Packages, Opts} = collect_args(install, Args),
    sutro:install(Packages, Opts);
    
dispatch(["uninstall" | Args]) ->
    {Packages, Opts} = collect_args(uninstall, Args),
    sutro:uninstall(Packages, Opts);
    
dispatch(["update" | Args]) ->
    {Packages, Opts} = collect_args(update, Args),
    case proplists:get_value(update_system, Opts) of
        true ->
            sutro:update_sutro(Opts);
        false ->
            sutro:update(Packages, Opts)
    end;
    
dispatch(["list" | Args]) ->
    {_, Opts} = collect_args(list, Args),
    Packages = sutro:installed(Opts),
    io:format("--> ~w package(s) installed~n", [length(Packages)]),
    [begin
        io:format("--> ~s~n", [PackageName]),
        io:format("      version:  ~s~n", [proplists:get_value(app_vsn, Props, "")]),
        io:format("      date:     ~s~n", [sutro_util:format_datetime(proplists:get_value(date, Props))]),
        io:format("      source:   ~s~n", [proplists:get_value(url, Props, "")])
    end || {PackageName, Props} <- Packages];

dispatch(["config"]) ->
    Props = sutro:get_config(),
    io:format("--> config values~n"),
    [begin
        io:format("      ~s ~p~n", [string:left(atom_to_list(Key),14,$ ), Val])
    end || {Key, Val} <- Props];
    
dispatch(["config" | Args]) ->
    {_, Opts} = collect_args(config, Args),
    sutro:set_config([{list_to_atom(Key), Val} || {config_set, [Key, Val]} <- Opts]);
    
dispatch(["spec" | Args]) ->
    {_, Opts} = collect_args(spec, Args),
    [begin
        sutro:add_spec(PackageName, [{url, Url}], Opts)
    end || {add_spec, [PackageName, Url]} <- Opts];
    
dispatch(_) ->
    io:format("Usage: sutro commands~n~n"),
    io:format("    search <search string> {global options}~n~n"),
    io:format("    install [<package name>...] {global options}~n~n"),
    io:format("    uninstall [<package name>...] {global options}~n~n"),
    io:format("    update [<package name>...] {global options}~n~n"),
    io:format("    list {global options}~n~n"),
    io:format("    config [--set <key> <value>...]~n~n"),
    io:format("    global options:~n"),
    io:format("        --spec-dir <spec dir>~n"),    
    io:format("        --build-dir <build dir>~n"),
    io:format("        --install-dir <install dir>~n"),
    io:format("        --verbose~n").
    
collect_args(Target, Args) ->
    collect_args(Target, Args, [], []).
    
collect_args(_, [], Packages, Opts) -> 
    {lists:reverse(Packages), lists:reverse(Opts)};
    
collect_args(Target, [Arg | Rest], Packages, Opts) ->
	case parse_tag(Target, Arg) of
		undefined -> %% if not a tag then must be a project name
			collect_args(Target, Rest, [Arg|Packages], Opts);
		{Opt, 0} ->  %% opt with no trailing value
            collect_args(Target, Rest, Packages, [Opt|Opts]);
		{Opt, NumVals} when is_integer(NumVals) -> %% opt with trailing value(s)
			if
				length(Rest) < NumVals ->
					?EXIT("poorly formatted command",[]);
				true -> ok
			end,
			{Vals, Rest1} = lists:split(NumVals, Rest),
			Vals1 = 
				case Vals of
					[V] -> V;
					_ -> Vals
				end,
            collect_args(Target, Rest1, Packages, [{Opt, Vals1}|Opts])
	end.

parse_tag(update, "--system") -> {{update_system, true}, 0};
parse_tag(config, "--set") -> {config_set, 2};
parse_tag(spec, "--add") -> {add_spec, 2};
parse_tag(_, "--verbose") -> {{verbose, true}, 0};
parse_tag(_, "--spec-dir") -> {spec_dir, 1};
parse_tag(_, "--" ++ Cmd) -> {list_to_atom(Cmd), 1};
parse_tag(_, _) -> undefined.