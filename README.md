# <http://sutro.heroku.com>

### Install

	$ curl "http://github.com/JacobVorreuter/sutro/raw/master/sutro" > sutro
	$ chmod +x sutro
	$ sudo mv sutro /usr/local/bin/

### Config

	$ sutro config
	sutro v0.1.0

	--> creating config file: /Users/jkvor/.sutroconfig
	        [{build_dir,"/Users/jkvor/.sutro/tmp"},
	         {install_dir,"/Users/jkvor/.sutro/lib"},
	         {spec_dir,"/Users/jkvor/.sutro/specs"}].

	--> config values
	      build_dir      "/Users/jkvor/.sutro/tmp"
	      install_dir    "/Users/jkvor/.sutro/lib"
	      spec_dir       "/Users/jkvor/.sutro/specs"
	
### Fetch spec files

	$ sutro update --system
	sutro v0.1.0

	--> fetching http://github.com/JacobVorreuter/sutro/tarball/master
	--> writing spec file: emongo.spec
	--> writing spec file: etap.spec
	--> writing spec file: syslog.spec
	--> updated sutro (/usr/local/bin/sutro) to latest version

### Commands

	sutro search emongo
	sutro install emongo
	sutro update emongo
	sutro uninstall emongo
	sutro list
	sutro config
	sutro help
	
### Do stuff

	$ sutro install emongo
	sutro v0.1.0

	--> fetching http://github.com/JacobVorreuter/emongo/tarball/v0.2
	--> running emongo build command
	--> running emongo install command
	--> fetching http://github.com/ngerakines/etap/tarball/master
	--> running etap build command
	--> running etap install command
	
	$ sutro list
	sutro v0.1.0

	--> 2 package(s) installed
	--> emongo
	      version:  0.2
	      date:     2010-04-06 13:42:51
	      source:   http://github.com/JacobVorreuter/emongo/tarball/v0.2
	--> etap
	      version:  
	      date:     2010-04-06 13:42:53
	      source:   http://github.com/ngerakines/etap/tarball/master
	
### Spec files

emongo.spec:

	{url, "http://github.com/JacobVorreuter/emongo/tarball/v0.2"}.
	{deps, [etap]}.
	