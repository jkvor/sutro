## Install
	
global config: ~/.sutroconfig
build directory: ~/.sutro/tmp
install directory: ~/.sutro/lib
default spec location: ~/.sutro/specs

## Config

~/.sutroconfig:
	
	{build_dir, "/Users/jkvor/.sutro/tmp"}.
	{install_dir, "/Users/jkvor/.sutro/lib"}.
	{spec_dir, "/Users/jkvor/specs"}.
	{spec_dir, "/Users/jkvor/.sutro/specs"}.

## Commands

	sutro search emongo
	sutro install emongo
	sutro update emongo
	sutro uninstall emongo
	sutro list
	sutro help
	
## Spec file

emongo.sutro:

	{url, "http://github.com/JacobVorreuter/emongo/tarball/v0.2"}.
	{deps, [etap]}.
	