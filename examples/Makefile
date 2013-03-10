export OCAMLPATH:=$(OCAMLPATH):../lib

CAMLP4=
OCAMLC_OPTS=-package riakc
OCAMLOPT_OPTS=$(OCAMLC_OPTS)

cources=ping.ml

sources=$(cources)

sources_mli=$(cources:%.ml=%.mli)

native_cmx=$(sources:%.ml=%.cmx)

byte_cmo=$(sources:%.ml=%.cmo)

native_cmi=$(sources_mli:%.mli=%.cmi)

.PHONY: all native byte native-code byte-code test clean

all: native byte

native-code: native

native: ping

ping: ping.cmx
	ocamlfind ocamlopt $(OCAMLOPT_OPTS) -thread -linkpkg -o $@ $^

byte-code: byte

byte: ping.byte

ping.byte: ping.cmo
	ocamlfind ocamlc $(OCAMLC_OPTS) -thread -custom -linkpkg -o $@ $^

test:

clean:
	-rm -f ping ping.byte *.cmx *.cmo

# Dependencies
include ../lib/Ocamlrules.mk.in
