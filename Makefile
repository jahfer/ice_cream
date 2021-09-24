FORMAT ?= exe
FILE ?= data/test_kitchen_sink.rb
DIR_RBS ?= data/rbs

build:
	dune build bin/cli.$(FORMAT)

run:
	OCAMLRUNPARAM=b dune exec bin/cli.$(FORMAT) -- --import-rbs=$(DIR_RBS) $(FILE) 

test:
	dune runtest

clean:
	dune clean

.PHONY: build clean run test