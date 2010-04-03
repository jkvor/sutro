-module(grak_setup).
-export([global_opts/1, ensure_setup/1]).
-include("grak.hrl").

global_opts(CmdLineOpts) ->
    ConfigVals = grak_util:config_file(),
    global_opts(CmdLineOpts, ConfigVals).
    
global_opts([], ConfigVals) -> ConfigVals;

global_opts([{Key, Val}|Tail], ConfigVals) ->
    ConfigVals1 = 
        case allowed_multi(Key) of
            true -> 
                [{Key, Val}|ConfigVals];
            false -> 
                lists:keystore(Key, 1, ConfigVals, {Key, Val})
        end,
    global_opts(Tail, ConfigVals1).
    
ensure_setup(Opts) ->
    case proplists:get_value(build_dir, Opts) of
        undefined -> ?EXIT(".grakconfig file is missing a build_dir parameter", []);
        BuildDir -> ensure_dir(BuildDir)
    end,
    
    case proplists:get_value(install_dir, Opts) of
        undefined -> ?EXIT(".grakconfig file is missing an install_dir parameter", []);
        InstallDir -> ensure_dir(InstallDir)
    end,
    
    case proplists:get_all_values(spec_dir, Opts) of
        [] -> ?EXIT(".grakconfig file is missing a spec_dir parameter", []);
        Specs -> [ensure_dir(SpecDir) || SpecDir <- Specs]
    end, 
    
    ok.

allowed_multi(spec_dir) -> true;
allowed_multi(_) -> false.
    
ensure_dir(Dir) ->
    case filelib:ensure_dir(Dir) of
        ok -> 
            case filelib:is_dir(Dir) of
                true -> ok;
                false ->
                    case file:make_dir(Dir) of
                        ok -> ok;
                        Error1 ->
                            ?EXIT("failed to create directory ~s: ~p", [Dir, Error1])
                    end
            end;
        Error -> 
            ?EXIT("failed to create directory ~s: ~p", [Dir, Error])
    end.