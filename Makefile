DUNE := dune

.PHONY: all
all: build

.PHONY: build
build:
	$(DUNE) build @all

.PHONY: sudoku
sudoku:
	$(DUNE) exec bin/main.exe

.PHONY: clean
clean:
	$(DUNE) clean

.PHONY: test
test:
	$(DUNE) runtest

.PHONY: fmt
fmt:
	ocamlformat -i lib/*.ml
	@if ls lib/*.mli 1> /dev/null 2>&1; then \
		ocamlformat -i lib/*.mli; \
	fi

