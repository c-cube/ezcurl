
all: build test

build:
	@dune build @all

test:
	@dune runtest --no-buffer --force

clean:
	@dune clean

doc:
	@dune build @doc

WATCH?=@all
watch:
	@dune build $(WATCH )-w

.PHONY: all build test watch
