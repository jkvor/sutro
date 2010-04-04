-module(grak).
-export([main/1, search/2, install/2]).
-include("grak.hrl").
    
%%====================================================================
%% main
%%====================================================================
main(Args) ->
	io:format("grackle v~s~n", [?APP_VSN]),
    grak_api:dispatch(Args).
    
%%====================================================================
%% search
%%====================================================================
search(SearchStr, Opts) ->
    grak_setup:run(Opts),
    case SearchStr of
        "" -> [];
        _ ->
            {ok, MP} = re:compile(SearchStr),
            search_spec_dirs(MP, get(spec_dirs), [])
    end.

%%====================================================================
%% install
%%====================================================================
install([Char|_]=PackageName, Opts) when is_integer(Char), is_list(Opts) ->
    install([PackageName], Opts);

install(Packages, Opts) when is_list(Packages), is_list(Opts) ->
    grak_setup:run(Opts),
    Specs = package_specs(Packages, []),
    Deps = expand_dependencies(Specs),
    do_install(Deps).
    
%%====================================================================
%% internal functions
%%====================================================================
search_spec_dirs(_, [], Acc) -> Acc;

search_spec_dirs(Regexp, [SpecDir|Tail], Results) ->
    Filenames = files_from(SpecDir),
    Results1 =
        lists:foldl(
            fun(Filename, Acc) ->
                Basename = filename:basename(Filename, ".spec"),
                case re:run(Basename, Regexp) of
                    nomatch -> Acc;
                    _ ->
                        SpecFile = filename:join([SpecDir, Filename]),
                        case file:consult(SpecFile) of
                            {ok, Props} ->
                                [{Basename, Props}|Acc];
                            Error ->
                                ?EXIT("failed to read spec file ~s: ~p", [SpecFile, Error])
                        end
                end
            end, Results, Filenames),
    search_spec_dirs(Regexp, Tail, Results1).
        
files_from(Dir) ->
    case file:list_dir(Dir) of
        {ok, Filenames} ->
            Filenames;
        Error ->
            ?EXIT("failed to read contents of directory ~s: ~p", [Dir, Error])
    end.
    
package_specs([], Acc) -> Acc;
    
package_specs([PackageName|Tail], Acc) ->
    case package_specs1(PackageName, get(spec_dirs)) of
        undefined ->
            package_specs(Tail, Acc);
        Props ->
            package_specs(Tail, [{PackageName, Props}|Acc])
    end.
    
package_specs1(PackageName, SpecDirs) ->
    case file:path_consult(SpecDirs, PackageName ++ ".spec") of
        {ok, Props, _} -> Props;
        _ -> undefined
    end.

expand_dependencies(Specs) ->
    G = digraph:new(),
    expand_dependencies(Specs, G),
    Deps = digraph_utils:topsort(G),
    digraph:delete(G),
    Deps.
    
expand_dependencies([], _) -> ok;

expand_dependencies([{PackageName, Props}|Tail], G) ->
    digraph:add_vertex(G, {PackageName, Props}),
    
    Deps = proplists:get_value(deps, Props, []),
    [begin
        DepPackageName = atom_to_list(Dep),
        case package_specs([DepPackageName], []) of
            [{_, DepProps}]=Specs ->
                digraph:add_edge(G, {PackageName, Props}, {DepPackageName, DepProps}),
                case digraph_utils:is_acyclic(G) of
                    true ->
                        expand_dependencies(Specs, G);
                    false ->
                        ?EXIT("circular dependency detected: ~s <--> ~p", [PackageName, Dep])
                end;
            [] ->
                ?EXIT("failed to locate spec file for dependency: ~s", [DepPackageName])
        end
     end || Dep <- Deps],
     
     expand_dependencies(Tail, G).
    
do_install([]) -> ok;

do_install([{PackageName, Props}|Tail]) ->
    case install_details(PackageName) of
        undefined ->
            Url = proplists:get_value(url, Props),
            Bin = download_tarball(Url),
            Hash = grak_util:hash_to_uint(Bin),
            UnpackDir = unpack_tarball(Bin),
            grak_util:set_cwd(UnpackDir),

        	prebuild({PackageName, Props}),
        	build({PackageName, Props}),
        	test({PackageName, Props}),
        	deploy({PackageName, [{hash, Hash}|Props]});
        _InstalledProps ->
            io:format("skipping ~s: package is already installed", [PackageName])
    end,
	do_install(Tail).
    
download_tarball(Url) ->
    io:format("downloading ~s~n", [Url]),
    case (catch http:request(get, {Url, [{"User-Agent", "Grackle"}]}, [{timeout, 6000}], [{body_format, binary}])) of
        {ok,{{_,200,_},_,Bin}} ->
            Bin;
        {ok, {{_,404,_},_,_}} ->
            ?EXIT("downloading tarball failed: not found (~s)", [Url]);
        Error ->
            ?EXIT("downloading tarball failed: ~p (~s)", [Error, Url])
    end.
    
unpack_tarball(Bin) ->
    case erl_tar:extract({binary, Bin}, [compressed, memory]) of
        {ok, Files} -> 
            PackageDir = lists:foldl(
                fun({Filename, _}, UnpackDir0) ->
                    case filename:split(Filename) of
                        [RootDir | _] when UnpackDir0 == RootDir; UnpackDir0 == undefined ->
                            RootDir;
                        [RootDir | _] ->
                            ?EXIT("poorly formatted tarball: multiple root-level directories (~s, ~s)", [UnpackDir0, RootDir])
                    end
                end, undefined, Files),
            UnpackDir = filename:join([get(build_dir), PackageDir]),
            grak_util:del_dir(UnpackDir),
            [begin
                Path = filename:join([get(build_dir), Filename]),
                filelib:ensure_dir(Path),
                file:write_file(Path, Contents)
            end || {Filename, Contents} <- Files],
            UnpackDir;
        {error, Reason} ->
            ?EXIT("failed to extract tarball: ~p", [Reason])
    end.
    
prebuild({ProjectName, Props}) ->
    case proplists:get_value(prebuild_command, Props) of
        undefined -> ok;
        PrebuildCmd ->
	        io:format("+ running ~s prebuild command~n", [ProjectName]),
        	grak_util:print_cmd_output("~s~n", [PrebuildCmd]),
        	grak_util:do_cmd(PrebuildCmd, fail)
    end.

build({ProjectName, Props}) ->
	case proplists:get_value(build_command, Props) of
		undefined ->
		    case filelib:is_regular("Emakefile") of
		        true ->
		            build1(ProjectName, "erl -make");
		        false ->
		            case filelib:is_regular("Makefile") of
        				true ->
        					build1(ProjectName, "make");
        				false ->
        					case os:find_executable("rebar") of
        						false ->
        							?EXIT("failed to build ~s: No Emakefile or Makefile found", [ProjectName]);
        						RebarExec ->
        							io:format("+ compiling with rebar...~n"),
        							build1(ProjectName, RebarExec ++ " compile")
        					end
        			end
		    end;
		Cmd ->
			build1(ProjectName, Cmd)
	end.
	
build1(ProjectName, BuildCmd) ->
	io:format("+ running ~s build command~n", [ProjectName]),
	grak_util:print_cmd_output("~s~n", [BuildCmd]),
	grak_util:do_cmd(BuildCmd, fail).

test({ProjectName, Props}) ->
    case proplists:get_value(test_command, Props) of
		undefined -> ok;
		TestCmd ->
		    io:format("+ running ~s test command~n", [ProjectName]),
        	grak_util:print_cmd_output("~s~n", [TestCmd]),
        	grak_util:do_cmd(TestCmd, fail)
	end.

deploy({ProjectName, Props}) ->
	Vsn = 
	    case file:consult(filename:join(["ebin", ProjectName ++ ".app"])) of
    		{ok,[{application,_,AppProps}]} -> proplists:get_value(vsn, AppProps);
    		_ -> undefined
    	end,
    Dir = 
        case Vsn of
            undefined -> filename:join([get(install_dir), ProjectName]);
            _ -> filename:join([get(install_dir), ProjectName ++ "-" ++ Vsn])
        end,
    
    {ok, FD} = file:open(ProjectName ++ ".spec", [write]),
    io:format(FD, "{date, ~p}.~n", [erlang:universaltime()]),
    [io:format(FD, "~p.~n", [Tuple]) || Tuple <- Props],
    file:close(FD),
    
	InstallCmd = "mkdir -p " ++ Dir ++ "; cp -R ./* " ++ Dir,
	io:format("+ running ~s install command~n", [ProjectName]),
	grak_util:print_cmd_output("~s~n", [InstallCmd]),
	grak_util:do_cmd(InstallCmd, fail),
	Ebin = filename:join([Dir, "ebin"]),
	case code:add_pathz(Ebin) of
	    true ->
	        ok;
	    Err ->
	        ?EXIT("failed to add path for ~s (~s): ~p", [ProjectName, Ebin, Err])
	end.

install_details(PackageName) ->    
    SpecDirs = filelib:wildcard("*", get(install_dir)),
    package_specs1(PackageName, [filename:join([get(install_dir), Dir]) || Dir <- SpecDirs]).
    