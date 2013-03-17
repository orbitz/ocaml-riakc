export OCAMLPATH:=$(OCAMLPATH):../lib

CAMLP4=
OCAMLC_OPTS=-package riakc
OCAMLOPT_OPTS=$(OCAMLC_OPTS)

cources=ping.ml client_id.ml server_info.ml \
	list_buckets.ml list_keys.ml bucket_props.ml \
	get.ml put.ml delete.ml

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

ping.native: ping.cmx

client_id.native: client_id.cmx

server_info.native: server_info.cmx

list_buckets.native: list_buckets.cmx

list_keys.native: list_keys.cmx

bucket_props.native: bucket_props.cmx

get.native: get.cmx

put.native: put.cmx

delete.native: delete.cmx

byte-code: byte

byte: $(byte_bin)

ping.byte: ping.cmo

client_id.byte: client_id.cmo

server_info.byte: server_info.cmo

list_buckets.byte: list_buckets.cmo

list_keys.byte: list_keys.cmo

bucket_props.byte: bucket_props.cmo

get.byte: get.cmo

put.byte: put.cmo

delete.byte: delete.cmo

test:

clean:
	-rm -f *.native *.byte *.cmx *.cmo *.o *.cmi

# Dependencies
include ../lib/Ocamlrules.mk.in
