FORMAT ?= exe
DIR ?= data/example

build:
	dune build bin/cli.$(FORMAT)

run:
	OCAMLRUNPARAM=b dune exec bin/cli.$(FORMAT) -- --dir=$(DIR)

check:
	OCAMLRUNPARAM=b dune exec bin/cli.$(FORMAT) -- --dir=$(DIR) --check 

test:
	dune runtest

clean:
	dune clean

.PHONY: build clean run test check