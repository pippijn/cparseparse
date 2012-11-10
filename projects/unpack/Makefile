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
-include /usr/local/share/pbuild/common.mk

view: build
	camlp4o nums.cma bigarray.cma sexplib/sexplib.cma camlp4/Camlp4Parsers/Camlp4GrammarParser.cmo _build/src/pa_unpack/pa_unpack.cma -printer Camlp4OCamlPrinter testsuite/pa_unpack/test.ml

check: build
	ocamlopt.opt -inline 999 -g -pp 'camlp4o -unsafe nums.cma bigarray.cma sexplib/sexplib.cma camlp4/Camlp4Parsers/Camlp4GrammarParser.cmo _build/src/pa_unpack/pa_unpack.cma' testsuite/pa_unpack/test.ml -o test
	./test
