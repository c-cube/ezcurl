
all: build test

build:
	@dune build @install

test:
	@dune runtest --no-buffer --force

test-autopromote:
	@dune runtest --no-buffer --force --auto-promote

clean:
	@dune clean

format:
	@dune build $(DUNE_OPTS) @fmt --auto-promote

format-check:
	@dune build $(DUNE_OPTS) @fmt --display=quiet

doc:
	@dune build @doc

WATCH?= @install @runtest
watch:
	@dune build $(WATCH )-w

VERSION=$(shell awk '/^version:/ {print $$2}' ezcurl.opam)

update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_VERSION/$(VERSION)/g" $(wildcard src/**/*.ml) $(wildcard src/**/*.mli)
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" $(wildcard src/**/*.ml) $(wildcard src/**/*.mli)

.PHONY: all build test watch update_next_tag
