
all: build test

build:
	@dune build @all

test:
	@dune runtest --no-buffer --force

clean:
	@dune clean

doc:
	@dune build @doc

watch:
	@dune build @all -w

.PHONY: all build test watch
