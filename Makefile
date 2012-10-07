elkhound/elkhound.native: setup.data
	ocamlbuild $@ ccparse/ccparse.native

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
setup.data: setup.ml
setup.ml: _oasis
	oasis setup

check: elkhound/elkhound.native
	_build/elkhound/elkhound.native | sed -e 's/\[[^m]*m//g' > o.txt


CALLGRIND = valgrind --tool=callgrind --dump-instr=yes --trace-jump=yes --instr-atstart=no

profile-elkhound: elkhound/elkhound.native
	rm -f callgrind.out.*
	$(CALLGRIND) _build/elkhound/elkhound.native
	rm -f callgrind.out
	mv callgrind.out.* callgrind.out

profile-ccparse: ccparse/ccparse.native
	rm -f callgrind.out.*
	$(CALLGRIND) _build/ccparse/ccparse.native testsuite/ccparse/profile.cc
	rm -f callgrind.out
	mv callgrind.out.* callgrind.out
