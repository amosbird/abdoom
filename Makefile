mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
cdir := $(dir $(mkfile_path))
install:
	@ln -sf $(cdir)email ../
	@ln -sf $(cdir)org ../
	@ln -sf $(cdir)cc ../
	@ln -sf $(cdir)doominit.el ../../../init.el

# Local Variables:
# compile-command: "make install"
# End:
