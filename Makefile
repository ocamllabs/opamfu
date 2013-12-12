.PHONY: build install uninstall reinstall clean

FINDLIB_NAME=opamfu
LIB_NAME=opamfu
BUILD=_build/lib

HAS_CMDLINER := $(shell ocamlfind query cmdliner > /dev/null; echo $$?)

ifneq ($(HAS_CMDLINER),0)
MLI=lib/*.mli _build/lib/*.cmi
FLAGS=-pkgs opam.client,uri
EXTRA_LIB=
EXTRA_META=
EXTRA_INSTALL=
else
MLI=lib/*.mli ui/*.mli _build/lib/*.cmi _build/ui/*.cmi
FLAGS=-pkgs opam.client,uri,cmdliner -I ui \
opamfuCli.cma opamfuCli.cmxa opamfuCli.a
EXTRA_LIB=
EXTRA_META=ui/opamfuCli.META
EXTRA_INSTALL= \
_build/ui/opamfuCli.cma _build/ui/opamfuCli.cmxa _build/ui/opamfuCli.a
endif

build: META lib/opamfu.mllib
	ocamlbuild -use-ocamlfind $(FLAGS) -I lib \
		$(LIB_NAME).cma $(LIB_NAME).cmxa $(LIB_NAME).a

install:
	ocamlfind install $(FINDLIB_NAME) META \
		$(MLI) \
		$(BUILD)/$(LIB_NAME).cma \
		$(BUILD)/$(LIB_NAME).cmxa \
		$(BUILD)/$(LIB_NAME).a \
		$(EXTRA_INSTALL)

META: META.in $(EXTRA_META)
	cat META.in $(EXTRA_META) > META

lib/opamfu.mllib: lib/opamfu.mllib.in
	cp lib/opamfu.mllib.in lib/opamfu.mllib
	echo $(EXTRA_LIB) >> lib/opamfu.mllib

uninstall:
	ocamlfind remove $(FINDLIB_NAME)

reinstall: uninstall install

clean:
	rm -rf _build
	rm -f lib/$(LIB_NAME).cm? META lib/opamfu.mllib
