export OCAMLPATH:=$(OCAMLPATH):../lib

CAMLP4=
OCAMLC_OPTS=-package riakc
OCAMLOPT_OPTS=$(OCAMLC_OPTS)

cources=ping.ml client_id.ml server_info.ml

sources=$(cources)

sources_mli=$(cources:%.ml=%.mli)

native_cmx=$(sources:%.ml=%.cmx)

byte_cmo=$(sources:%.ml=%.cmo)

native_cmi=$(sources_mli:%.mli=%.cmi)

.PHONY: all native byte native-code byte-code test clean

all: native byte

native-code: native

native: ping client_id server_info

ping: ping.cmx
	ocamlfind ocamlopt $(OCAMLOPT_OPTS) -thread -linkpkg -o $@ $^

client_id: client_id.cmx
	ocamlfind ocamlopt $(OCAMLOPT_OPTS) -thread -linkpkg -o $@ $^

server_info: server_info.cmx
	ocamlfind ocamlopt $(OCAMLOPT_OPTS) -thread -linkpkg -o $@ $^

byte-code: byte

byte: ping.byte client_id.byte server_info.byte

ping.byte: ping.cmo
	ocamlfind ocamlc $(OCAMLC_OPTS) -thread -custom -linkpkg -o $@ $^

client_id.byte: client_id.cmo
	ocamlfind ocamlc $(OCAMLC_OPTS) -thread -custom -linkpkg -o $@ $^

server_info.byte: server_info.cmo
	ocamlfind ocamlc $(OCAMLC_OPTS) -thread -custom -linkpkg -o $@ $^

test:

clean:
	-rm -f ping ping.byte client_id client_id.byte *.cmx *.cmo *.o

# Dependencies
include ../lib/Ocamlrules.mk.in
