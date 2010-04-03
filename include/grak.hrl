-define(EXIT(Format, Args), exit(lists:flatten(io_lib:format("- " ++ Format, Args)))).

-record(state, {build_dir, install_dir, spec_dirs}).