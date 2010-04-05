-module(sutro).
-export([main/1, search/2, install/2, uninstall/2, 
         update/2, installed/1, get_config/0, set_config/1]).
         
-include("sutro.hrl").
    
%%====================================================================
%% main
%%====================================================================
main(Args) ->
	io:format("sutro v~s~n~n", [?APP_VSN]),
	case (catch sutro_api:dispatch(Args)) of
	    {'EXIT', Error} ->
	        io:format("~s~n~n", [Error]);
	    _ ->
	        io:format("~n")
	end.
    
%%====================================================================
%% search
%%====================================================================
search(SearchStr, Opts) ->
    sutro_setup:run(Opts),
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
    sutro_setup:run(Opts),
    Specs = package_specs(Packages, []),
    Deps = expand_dependencies(Specs),
    do_install(Deps).
    
%%====================================================================
%% uninstall
%%====================================================================
uninstall([Char|_]=PackageName, Opts) when is_integer(Char), is_list(Opts) ->
    uninstall([PackageName], Opts);

uninstall(Packages, Opts) when is_list(Packages), is_list(Opts) ->
    sutro_setup:run(Opts),
    do_uninstall(Packages).

%%====================================================================
%% update
%%====================================================================
update([Char|_]=PackageName, Opts) when is_integer(Char), is_list(Opts) ->
    update([PackageName], Opts);

update(Packages, Opts) when is_list(Packages), is_list(Opts) ->
    sutro_setup:run(Opts),
    do_update(Packages).
    
%%====================================================================
%% installed
%%====================================================================
installed(Opts) ->
    sutro_setup:run(Opts),
    installed_packages().

%%====================================================================
%% config
%%====================================================================
get_config() ->
    sutro_setup:run([]),
    sutro_util:config_file().
    
set_config(Values) ->
    sutro_setup:run([]),
    Config = lists:foldl(
        fun({Key, Val}, Acc) ->
            io:format("--> setting config value~n"),
            io:format("       ~p = ~p~n", [Key, Val]),
            sutro_util:add_config(Key, Val, Acc)
        end, sutro_util:config_file(), Values),
    sutro_util:write_config(sutro_util:config_path(), Config).
    
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
        {_, Props} ->
            package_specs(Tail, [{PackageName, Props}|Acc])
    end.
    
package_specs1(PackageName, SpecDirs) ->
    case file:path_consult(SpecDirs, PackageName ++ ".spec") of
        {ok, Props, Filename} -> {Filename, Props};
        _ -> undefined
    end.

expand_dependencies(Specs) ->
    G = digraph:new(),
    expand_dependencies(Specs, G),
    Deps = lists:reverse(digraph_utils:topsort(G)),
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
            Hash = sutro_util:hash_to_uint(Bin),
            UnpackDir = unpack_tarball(Bin),
            sutro_util:set_cwd(UnpackDir),

        	prebuild({PackageName, Props}),
        	build({PackageName, Props}),
        	test({PackageName, Props}),
        	deploy({PackageName, [{hash, Hash}|Props]});
        {_Filename, _InstalledProps} ->
            io:format("--> skipping ~s: package is already installed", [PackageName])
    end,
	do_install(Tail).

do_uninstall([]) -> ok;
	
do_uninstall([PackageName|Tail]) ->
    case install_details(PackageName) of
        undefined ->
            io:format("--> skipping ~s: not installed~n", [PackageName]);
        {SpecFileName, _Props} ->
            %% TODO: remove packages that depend on this one first
            InstallDir = filename:dirname(SpecFileName),
            io:format("--> removing directory ~s~n", [InstallDir]),
            sutro_util:del_dir(InstallDir)
    end,
    do_uninstall(Tail).

do_update([]) -> ok;

do_update([PackageName|Tail]) ->
    case install_details(PackageName) of
        undefined ->
            io:format("--> skipping ~s: not installed~n", [PackageName]);
        {SpecFileName, InstalledProps} ->
            case package_specs1(PackageName, get(spec_dirs)) of
                {_Filename, Props} ->
                    Url = proplists:get_value(url, Props),
                    Bin = download_tarball(Url),
                    Hash = sutro_util:hash_to_uint(Bin),
                    case proplists:get_value(hash, InstalledProps) of
                        Hash ->
                            io:format("--> skipping ~s: up-to-date~n", [PackageName]);
                        _ ->
                            io:format("--> updating ~s~n", [PackageName]),
                            InstallDir = filename:dirname(SpecFileName),
                            sutro_util:del_dir(InstallDir),
                            
                            UnpackDir = unpack_tarball(Bin),
                            sutro_util:set_cwd(UnpackDir),

                        	prebuild({PackageName, Props}),
                        	build({PackageName, Props}),
                        	test({PackageName, Props}),
                        	deploy({PackageName, [{hash, Hash}|Props]})
                    end;
                undefined ->
                    ?EXIT("failed to locate spec file for ~s", [PackageName])
            end
    end,
    do_update(Tail).
    
installed_packages() ->
    [begin
        case file:consult(filename:join([get(install_dir), Filename])) of
            {ok, Props} -> {filename:basename(Filename, ".spec"), Props};
            Error -> ?EXIT("failed to read installed package spec file: ~p (~s)", [Error, Filename])
        end
    end || Filename <- filelib:wildcard("*/*.spec", get(install_dir))].
        
download_tarball(Url) ->
    io:format("--> fetching ~s~n", [Url]),
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
            sutro_util:del_dir(UnpackDir),
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
	        io:format("--> running ~s prebuild command~n", [ProjectName]),
        	sutro_util:print_cmd_output("~s~n", [PrebuildCmd]),
        	sutro_util:do_cmd(PrebuildCmd, fail)
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
        							io:format("--> compiling with rebar...~n"),
        							build1(ProjectName, RebarExec ++ " compile")
        					end
        			end
		    end;
		Cmd ->
			build1(ProjectName, Cmd)
	end.
	
build1(ProjectName, BuildCmd) ->
	io:format("--> running ~s build command~n", [ProjectName]),
	sutro_util:print_cmd_output("~s~n", [BuildCmd]),
	sutro_util:do_cmd(BuildCmd, fail).

test({ProjectName, Props}) ->
    case proplists:get_value(test_command, Props) of
		undefined -> ok;
		TestCmd ->
		    io:format("--> running ~s test command~n", [ProjectName]),
        	sutro_util:print_cmd_output("~s~n", [TestCmd]),
        	sutro_util:do_cmd(TestCmd, fail)
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
    io:format(FD, "{date, ~p}.~n", [erlang:localtime()]),
    if Vsn=/=undefined -> io:format(FD, "~p.~n", [{app_vsn, Vsn}]); true -> ok end,
    [io:format(FD, "~p.~n", [Tuple]) || Tuple <- Props],
    file:close(FD),
    
	InstallCmd = "mkdir -p " ++ Dir ++ "; cp -R ./* " ++ Dir,
	io:format("--> running ~s install command~n", [ProjectName]),
	sutro_util:print_cmd_output("~s~n", [InstallCmd]),
	sutro_util:do_cmd(InstallCmd, fail),
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
    