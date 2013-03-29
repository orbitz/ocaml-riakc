export OCAMLPATH:=$(OCAMLPATH):../lib

CAMLP4=
OCAMLC_OPTS=-package riakc
OCAMLOPT_OPTS=$(OCAMLC_OPTS)

NON_LIB_MODULES=all_tests.ml

native_bin=$(NON_LIB_MODULES:%.ml=%.native)

byte_bin=$(NON_LIB_MODULES:%.ml=%.byte)

.PHONY: all native-code byte-code test clean

all: native-code byte-code

native-code: $(native_bin)

all_tests.native: all_tests.cmx

byte-code: $(byte_bin)

all_tests.byte: all_tests.cmo

test: all
	./all_tests.native $(RIAK_HOST) $(RIAK_PORT) $(RIAK_BUCKET)

clean:
	-rm -f *.native *.byte *.cmx *.cmo *.o *.cmi

# Dependencies
include ../lib/Ocamlrules.mk.in
