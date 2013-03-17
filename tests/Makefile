export OCAMLPATH:=$(OCAMLPATH):../lib

CAMLP4=
OCAMLC_OPTS=-package riakc
OCAMLOPT_OPTS=$(OCAMLC_OPTS)

cources=all_tests.ml

sources=$(cources)

sources_mli=$(cources:%.ml=%.mli)

native_cmx=$(sources:%.ml=%.cmx)

byte_cmo=$(sources:%.ml=%.cmo)

native_cmi=$(sources_mli:%.mli=%.cmi)

native_bin=$(cources:%.ml=%.native)

byte_bin=$(cources:%.ml=%.byte)

.PHONY: all native byte native-code byte-code test clean

all: native byte

native-code: native

native: $(native_bin)

all_tests.native: all_tests.cmx

byte-code: byte

byte: $(byte_bin)

all_tests.byte: all_tests.cmo

test: all

clean:
	-rm -f *.native *.byte *.cmx *.cmo *.o *.cmi

# Dependencies
include ../lib/Ocamlrules.mk.in
