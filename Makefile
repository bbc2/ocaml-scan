all: build

.PHONY: build
build:
	dune build scan.exe

.PHONY: clean
clean:
	dune clean
