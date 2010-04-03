## Install
	
global config: ~/.grakconfig
build directory: ~/.grackle/tmp
install directory: ~/.grackle/lib
default spec location: ~/.grackle/specs

## Config

~/.grakconfig:
	
	{build_dir, "/Users/jkvor/.grackle/tmp"}.
	{install_dir, "/Users/jkvor/.grackle/lib"}.
	{spec_dir, "/Users/jkvor/specs"}.
	{spec_dir, "/Users/jkvor/.grackle/specs"}.

## Commands

	grak search emongo
	grak install emongo
	grak update emongo
	grak uninstall emongo
	grak list
	grak help
	
## Spec file

emongo.grak:

	{url, "http://github.com/JacobVorreuter/emongo/tarball/v0.2"}.
	{deps, [etap]}.
	