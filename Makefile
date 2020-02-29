.PHONY: lint
lint:
	dune build @check

.PHONY: check-format
check-format:
	dune build @fmt

.PHONY: format
format:
	dune build @fmt --auto-promote

.PHONY: check
check: lint check-format
