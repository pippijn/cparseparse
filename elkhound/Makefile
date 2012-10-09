# OASIS_START
# DO NOT EDIT (digest: bc1e05bfc8b39b664f29dae8dbd3ebbb)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all: 
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean: 
	$(SETUP) -clean $(CLEANFLAGS)

distclean: 
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP
TARGETS =				\
	elkhound/elkhound.native	\
	ccparse/ccparse.native		\
	runtests.native

default: setup.data
	ocamlbuild $(TARGETS)

%.native: setup.data
	ocamlbuild $@

setup.data: setup.ml
setup.ml: _oasis
	oasis setup

check: default
	_build/runtests.native

check-elkhound: elkhound/elkhound.native
	_build/elkhound/elkhound.native | sed -e 's/\[[^m]*m//g' > o.txt

check-ccparse: ccparse/ccparse.native
	_build/ccparse/ccparse.native -ptree testsuite/c++11.ii


CALLGRIND = valgrind --tool=callgrind		\
	--callgrind-out-file=callgrind.out.1	\
	--dump-line=no				\
	--dump-instr=yes			\
	--collect-jumps=yes			\
	--instr-atstart=no

profile-elkhound: elkhound/elkhound.native
	$(CALLGRIND) _build/elkhound/elkhound.native

profile-ccparse: ccparse/ccparse.native
	$(CALLGRIND) _build/ccparse/ccparse.native -pp -utf8 -trivial testsuite/ccparse/profile.cc
