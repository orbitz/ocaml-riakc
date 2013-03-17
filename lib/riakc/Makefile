CAMLP4=
OCAMLC_OPTS=-package core,async,async_core,async_unix,bitstring,bitstring.syntax,protobuf -syntax camlp4o
OCAMLOPT_OPTS=-for-pack Riakc $(OCAMLC_OPTS)

cources=pb_robj.ml pb_response.ml robj.ml request.ml response.ml opts.ml conn.ml

sources=$(cources)

sources_mli=$(cources:%.ml=%.mli)

native_cmx=$(sources:%.ml=%.cmx)

byte_cmo=$(sources:%.ml=%.cmo)

native_cmi=$(sources_mli:%.mli=%.cmi)

.PHONY: all install native byte native-code byte-code test clean

all: native byte

install: all
	ocamlfind install riakc META riakc.cmxa riakc.cma riakc.a \
	riakc.cmi $(native_cmx) $(native_cmi) $(sources_mli)

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

clean:
	-rm -vf *.cmxa *.cma *.cmo *.cmx *.cmi *.o *.a .d

# Dependencies
include ../Ocamlrules.mk.in
