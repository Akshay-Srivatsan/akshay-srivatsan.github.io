.PHONY: all
all: build

.PHONY: build
build:
	cabal run site -- build

.PHONY: rebuild
rebuild:
	cabal run site -- rebuild

.PHONY: clean
clean:
	cabal run site -- clean

.PHONY: serve
serve:
	cabal run site -- server

.PHONY: watch
watch:
	cabal run site -- watch

.PHONY: format
format:
	prettier --write .
