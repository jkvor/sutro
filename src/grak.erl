-module(grak).
-export([main/1, search/2, install/2]).
-include("grak.hrl").

-define(SETUP(Opts), 
    begin
        GOpts = grak_setup:global_opts(Opts),
        ok = grak_setup:ensure_setup(GOpts),
        #state{
            build_dir=proplists:get_value(build_dir, GOpts),
            install_dir=proplists:get_value(install_dir, GOpts),
            spec_dirs=proplists:get_all_values(spec_dir, GOpts)
        }
    end).

%%====================================================================
%% main
%%====================================================================
main(Args) ->
	io:format("grackle v~s~n", [?APP_VSN]),

    ok.
    
%%====================================================================
%% search
%%====================================================================
search(SearchStr, Opts) ->
    State = ?SETUP(Opts),
    case SearchStr of
        "" -> [];
        _ ->
            {ok, MP} = re:compile(SearchStr),
            search_spec_dirs(MP, State#state.spec_dirs, State, [])
    end.
    
%%====================================================================
%% install
%%====================================================================
install([Char|_]=PackageName, Opts) when is_integer(Char), is_list(Opts) ->
    install([PackageName], Opts);
    
install(Packages, Opts) when is_list(Packages), is_list(Opts) ->
    State = ?SETUP(Opts),
    package_specs(Packages, State).
    
package_specs([], _) -> undefined;

package_specs([PackageName|Tail], State) ->
    case file:path_consult(State#state.spec_dirs, PackageName ++ ".spec") of
        {ok, Props, _} -> Props;
        _ -> package_specs(Tail, State)
    end.
    
%%====================================================================
%% internal functions
%%====================================================================
search_spec_dirs(_, [], _, Acc) -> Acc;

search_spec_dirs(Regexp, [SpecDir|Tail], State, Results) ->
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
    search_spec_dirs(Regexp, Tail, State, Results1).
        
files_from(Dir) ->
    case file:list_dir(Dir) of
        {ok, Filenames} ->
            Filenames;
        Error ->
            ?EXIT("failed to read contents of directory ~s: ~p", [Dir, Error])
    end.    