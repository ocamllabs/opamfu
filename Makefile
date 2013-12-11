.PHONY: build install uninstall reinstall clean

FINDLIB_NAME=opamfu
MOD_NAME=opamfUniverse
BUILD=_build/lib
FLAGS=-pkgs opam.client,uri

build:
	ocamlbuild -use-ocamlfind $(FLAGS) -I lib \
		$(MOD_NAME).cma $(MOD_NAME).cmxa $(MOD_NAME).a

install:
	ocamlfind install $(FINDLIB_NAME) META \
		lib/$(MOD_NAME).mli \
		$(BUILD)/$(MOD_NAME).cmi \
		$(BUILD)/$(MOD_NAME).cma \
		$(BUILD)/$(MOD_NAME).cmxa \
		$(BUILD)/$(MOD_NAME).a

uninstall:
	ocamlfind remove $(FINDLIB_NAME)

reinstall: uninstall install

clean:
	rm -rf _build
	rm -f lib/$(MOD_NAME).cm?
