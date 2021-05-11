.PHONY: clean all dist test
.DEFAULT_GOAL := all

all:
	cabal v2-build

dist:
	cabal v2-sdist

test:
	cabal v2-test

clean:
	rm -rf dist*
