mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
cdir := $(dir $(mkfile_path))
install:
	@ln -sf $(cdir)email ../
	@ln -sf $(cdir)org ../
	@ln -sf $(cdir)org-capture ../
	@ln -sf $(cdir)cc ../
	@ln -sf $(cdir)python ../
	@ln -sf $(cdir)doominit.el ../../../init.el
	@mkdir -p ../../../.local/etc
	@ln -sf $(cdir)custom.el ../../../.local/etc/

# Local Variables:
# compile-command: "make install"
# End:
