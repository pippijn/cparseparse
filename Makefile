.SILENT:

PROJECTS = $(notdir $(wildcard projects/*))


default: setup build

########################################################################
## :: Rules
########################################################################

setup:		$(addprefix .setup.,	$(PROJECTS))	; $(RM) $^
build:		$(addprefix .build.,	$(PROJECTS))	; $(RM) $^
test:		$(addprefix .test.,	$(PROJECTS))	; $(RM) $^
clean:		$(addprefix .clean.,	$(PROJECTS))	; $(RM) $^
uninstall:	$(addprefix .uninstall.,$(PROJECTS))	; $(RM) $^

.setup.%:
	cd projects/$* && oasis setup -setup-update weak

.build.%:
	$(MAKE) -C projects/$* build reinstall

.test.%:
	-$(MAKE) -C projects/$* test

.clean.%:
	-$(MAKE) -C projects/$* clean

.uninstall.%:
	-$(MAKE) -C projects/$* uninstall


########################################################################
## :: Dependencies
########################################################################

.build.ccparse:			\
	.build.baselib		\
	.build.glr		\
	.build.treematch

.build.glr:			\
	.build.baselib		\
	.build.codegen		\
	.build.treematch

.build.re2ml:			\
	.build.baselib		\
	.build.codegen

.build.codegen:			\
	.build.baselib

.build.treematch:		\
	.build.baselib
