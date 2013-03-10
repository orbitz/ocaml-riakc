.PHONY: all examples clean test

all:
	$(MAKE) -C lib

examples: all
	$(MAKE) -C examples

test:
	$(MAKE) -C lib test

clean:
	$(MAKE) -C lib clean
	$(MAKE) -C examples clean

