
all: build test

build:
	@dune build @install

test:
	@dune runtest --no-buffer --force

clean:
	@dune clean

doc:
	@dune build @doc

WATCH?= @install @runtest
watch:
	@dune build $(WATCH )-w

.PHONY: all build test watch
