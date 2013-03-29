CAMLP4=
PACK=riakc
OCAMLC_OPTS=-package core,async,async_core,async_unix,bitstring,bitstring.syntax,protobuf \
	-syntax camlp4o
OCAMLOPT_OPTS=$(OCAMLC_OPTS)

LIB_MODULES=pb_robj.ml pb_response.ml robj.ml request.ml response.ml opts.ml conn.ml

BYTE_TARGETS=riakc.cma
NATIVE_TARGETS=riakc.cmxa

.PHONY: all install test

all: native-code byte-code

install: all
	ocamlfind install riakc META riakc.cmxa riakc.cma riakc.a *.cmx *.cmi *.mli

test:

# Dependencies
include ../Ocamlrules.mk.in
