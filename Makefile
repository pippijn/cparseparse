GR_MODS = gr/cc.gr gr/gnu.gr gr/kandr.gr
TOK_MODS = tok/cc_tokens.tok tok/gnu_ext.tok

main.native: cc.ml $(wildcard *.ml *.mll glr/*)
	ocamlbuild $@

cc.ml: $(GR_MODS) tok/cc_tokens.ids
	../sanaly/oink-stack/elkhound/elkhound -ocaml -v -tr lrtable -o cc $(GR_MODS)

tok/cc_tokens.ids: $(TOK_MODS) make-token-files
	./make-token-files $(TOK_MODS)
