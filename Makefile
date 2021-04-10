
FORMAT ?= exe
FILE ?= data/test_simple.rb

build:
	dune build bin/cli.$(FORMAT)

run:
	dune exec bin/cli.$(FORMAT) $(FILE)

test:
	dune runtest

clean:
	dune clean

.PHONY: build clean run test docs