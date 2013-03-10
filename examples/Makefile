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

native: ping.native client_id.native server_info.native

ping.native: ping.cmx

client_id.native: client_id.cmx

server_info.native: server_info.cmx

byte-code: byte

byte: ping.byte client_id.byte server_info.byte

ping.byte: ping.cmo

client_id.byte: client_id.cmo

server_info.byte: server_info.cmo

test:

clean:
	-rm -f *.native *.byte *.cmx *.cmo *.o

# Dependencies
include ../lib/Ocamlrules.mk.in
