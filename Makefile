mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
cdir := $(dir $(mkfile_path))
install:
	@ln -sf $(cdir)email ../
