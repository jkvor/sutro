-module(sutro_setup).
-export([run/1, global_opts/1, ensure_setup/1]).
-include("sutro.hrl").

run(Opts) ->
    case get(has_set_run) of
        true -> ok;
        _ ->
            GOpts = sutro_setup:global_opts(Opts),
            ok = sutro_setup:ensure_setup(GOpts),
            put(verbose, proplists:get_value(verbose, GOpts, false)),
            put(build_dir, proplists:get_value(build_dir, GOpts)),
            put(install_dir, proplists:get_value(install_dir, GOpts)),
            put(spec_dir, proplists:get_value(spec_dir, GOpts)),
            put(has_setup_run, true)
    end.
    
global_opts(CmdLineOpts) ->
    ConfigVals = sutro_util:config_file(),
    global_opts(CmdLineOpts, ConfigVals).
    
global_opts([], ConfigVals) -> ConfigVals;

global_opts([{Key, Val}|Tail], ConfigVals) ->
    ConfigVals1 = sutro_util:add_config(Key, Val, ConfigVals),
    global_opts(Tail, ConfigVals1).
    
ensure_setup(Opts) ->
    inets:start(),
    error_logger:tty(false),
    
    case proplists:get_value(build_dir, Opts) of
        undefined -> ?EXIT(".sutroconfig file is missing a build_dir parameter", []);
        BuildDir -> ensure_dir(BuildDir)
    end,
    
    case proplists:get_value(install_dir, Opts) of
        undefined -> ?EXIT(".sutroconfig file is missing an install_dir parameter", []);
        InstallDir -> ensure_dir(InstallDir)
    end,
    
    case proplists:get_value(spec_dir, Opts) of
        undefined -> ?EXIT(".sutroconfig file is missing a spec_dir parameter", []);
        SpecDir -> ensure_dir(SpecDir)
    end, 
    
    ok.
    
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