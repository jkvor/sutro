-module(sutro).
-export([main/1, search/1, install/1, uninstall/1, 
         update/1, installed/0, get_config/0, set_config/1,
         add_spec/2, available_packages/0]).
         
-compile(export_all).
         
-include("sutro.hrl").

-define(DOWNLOAD_TIMEOUT, 8000).
    
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
search(SearchStr) ->
    case SearchStr of
        "" -> [];
        _ ->
            {ok, MP} = re:compile(SearchStr),
            search_spec_dir(MP, get(spec_dir))
    end.

%%====================================================================
%% install
%%====================================================================
install([Char|_]=PackageName) when is_integer(Char) ->
    install([PackageName]);

install(Packages) when is_list(Packages) ->
    Specs = package_specs(Packages, []),
    Deps = expand_dependencies(Specs),
    do_install(Deps).
    
%%====================================================================
%% uninstall
%%====================================================================
uninstall([Char|_]=PackageName) when is_integer(Char) ->
    uninstall([PackageName]);

uninstall(Packages) when is_list(Packages) ->
    do_uninstall(Packages).

%%====================================================================
%% update
%%====================================================================
update([Char|_]=PackageName) when is_integer(Char) ->
    update([PackageName]);

update(Packages) when is_list(Packages) ->
    do_update(Packages).
    
update_sutro() ->
    do_update_sutro().
    
%%====================================================================
%% installed
%%====================================================================
installed() ->
    installed_packages().

%%====================================================================
%% available
%%====================================================================
available_packages() ->
    Filenames = files_from(get(spec_dir)),
    lists:foldl(
        fun(Filename, Acc) ->
            Basename = filename:basename(Filename, ".spec"),
            SpecFile = filename:join([get(spec_dir), Filename]),
            case file:consult(SpecFile) of
                {ok, Props} ->
                    [{Basename, Props}|Acc];
                Error ->
                    ?EXIT("failed to read spec file ~s: ~p", [SpecFile, Error])
            end
        end, [], Filenames).

%%====================================================================
%% config
%%====================================================================
get_config() ->
    sutro_util:config_file().
    
set_config(Values) ->
    Config = lists:foldl(
        fun({Key, Val}, Acc) ->
            io:format("--> setting config value~n"),
            io:format("       ~p = ~p~n", [Key, Val]),
            sutro_util:add_config(Key, Val, Acc)
        end, sutro_util:config_file(), Values),
    sutro_util:write_config(sutro_util:config_path(), Config).
    
%%====================================================================
%% spec
%%====================================================================    
add_spec(PackageName, Props) ->
    SpecFileName = filename:join([get(spec_dir), PackageName ++ ".spec"]),
    {ok, FD} = file:open(SpecFileName, [write]),
    [io:format(FD, "~p.~n", [Tuple]) || Tuple <- Props],
    file:close(FD),
    io:format("--> wrote spec file ~s~n", [SpecFileName]),
    ok.

%%====================================================================
%% internal functions
%%====================================================================
search_spec_dir(Regexp, SpecDir) ->
    Filenames = files_from(SpecDir),
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
        end, [], Filenames).
        
files_from(Dir) ->
    case file:list_dir(Dir) of
        {ok, Filenames} ->
            Filenames;
        Error ->
            ?EXIT("failed to read contents of directory ~s: ~p", [Dir, Error])
    end.
    
package_specs([], Acc) -> Acc;
    
package_specs([PackageName|Tail], Acc) ->
    case package_specs1(PackageName, [get(spec_dir)]) of
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
            io:format("--> skipping ~s: package is already installed~n", [PackageName])
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
            case package_specs1(PackageName, [get(spec_dir)]) of
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
    
do_update_sutro() ->
    SpecDir = get(spec_dir),
    Url = "http://github.com/JacobVorreuter/sutro/tarball/master",
    Bin = download_tarball(Url),
    case erl_tar:extract({binary, Bin}, [compressed, memory]) of
        {ok, Files} -> 
            [begin
                case filename:split(Filename) of
                    [_, "sutro"] ->
                        File = 
                    		case os:find_executable("sutro") of
                    			false ->
                    				case filelib:is_regular("sutro") of
                    					true -> "./sutro";
                    					fasle -> exit("failed to find sutro executable to replace")
                    				end;
                    			F -> F
                    		end,
                    		
                    		case file:write_file(File, Contents) of
                				ok ->
                					io:format("--> updated sutro (~s) to latest version~n", [File]);
                				{error, Reason} ->
                					exit(lists:flatten(io_lib:format("failed to overwrite sutro executable ~s: ~p~n", [File, Reason])))
                			end;
                    [_, "specs", SpecName] ->
                        io:format("--> writing spec file: ~s~n", [SpecName]),
                        file:write_file(filename:join([SpecDir, SpecName]), Contents);
                    _ ->
                        ok
                end
            end || {Filename, Contents} <- Files];
        {error, Reason} ->
            ?EXIT("failed to extract tarball: ~p", [Reason])
    end.
    
installed_packages() ->
    [begin
        case file:consult(filename:join([get(install_dir), Filename])) of
            {ok, Props} -> {filename:basename(Filename, ".spec"), Props};
            Error -> ?EXIT("failed to read installed package spec file: ~p (~s)", [Error, Filename])
        end
    end || Filename <- filelib:wildcard("*/*.spec", get(install_dir))].
        
download_tarball(Url) ->
    io:format("--> fetching ~s~n", [Url]),
    case (catch http:request(get, {Url, [{"User-Agent", "Grackle"}]}, [{timeout, ?DOWNLOAD_TIMEOUT}], [{body_format, binary}])) of
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
    