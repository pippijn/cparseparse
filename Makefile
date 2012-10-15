CONFIGUREFLAGS ?= --enable-tests

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
SETUP = _build/setup.native

setup.native: setup.ml
	ocamlbuild $@

setup.data: setup.native
setup.ml: _oasis
	oasis setup

check: test

check-elkhound: build
	./elkhound.native | sed -e 's/\[[^m]*m//g' > o.txt

check-cpapa: build
	./cpapa.native -ptree testsuite/c++11.ii


CALLGRIND = valgrind --tool=callgrind			\
	--callgrind-out-file=$(PWD)/callgrind.out.1	\
	--dump-line=no					\
	--dump-instr=yes				\
	--collect-jumps=yes				\
	--instr-atstart=no

profile-elkhound: build
	cd _build && $(CALLGRIND) elkhound/elkhound.native

profile-cpapa: build
	$(CALLGRIND) ./cpapa.native -pp -trivial testsuite/profile.cc
