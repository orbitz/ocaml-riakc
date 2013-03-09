CAMLP4=
OCAMLC_OPTS=-package core,async,async_core,async_unix,bitstring,bitstring.syntax,protobuf -syntax camlp4o
OCAMLOPT_OPTS=-for-pack Riakc $(OCAMLC_OPTS)

cources=robj.ml request.ml response.ml conn.ml

sources=$(cources)

sources_mli=$(cources:%.ml=%.mli)

native_cmx=$(sources:%.ml=%.cmx)

byte_cmo=$(sources:%.ml=%.cmo)

native_cmi=$(sources_mli:%.mli=%.cmi)

.PHONY: all native byte native-code byte-code test clean

all: native byte

native-code: native

native: riakc.cmxa

riakc.cmxa: riakc.cmx

riakc.cmx: $(native_cmx)
	ocamlfind ocamlopt -pack -o $@ $^

byte-code: byte

byte: riakc.cma

riakc.cma: riakc.cmo

riakc.cmo: $(byte_cmo)
	ocamlfind ocamlc -pack -o $@ $^

test:

native:

byte:

# Dependencies
include ../Ocamlrules.mk.in
