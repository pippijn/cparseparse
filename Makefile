-include config.mk

GR_MODS = gr/cc.gr gr/gnu.gr gr/kandr.gr
TOK_MODS = tok/cc_tokens.tok tok/gnu_ext.tok
ELKHOUND ?= elkhound

main.native: cc.ml dypcc.ml $(wildcard *.ml *.mll glr/*)
	ocamlbuild -cflags '-unsafe' -use-ocamlfind $@

dypcc.ml: dypcc.y
	dypgen --ocamlc '-I /usr/lib/ocaml/dyp' $<

cc.ml: $(GR_MODS) tok/cc_tokens.ids
	$(ELKHOUND) -ocaml -v -tr lrtable -o cc $(GR_MODS)

tok/cc_tokens.ids: $(TOK_MODS) make-token-files
	./make-token-files $(TOK_MODS)

test.ii: test.cpp
	g++-4.4 -E -P $< -o $@ -I /usr/include/qt4

check: test.ii main.native
	./main.native $<

profile: main.native
	rm -f callgrind.out.*
	valgrind --tool=callgrind ./main.native testsuite/profile.cc 2>&1 | grep "refs:"
