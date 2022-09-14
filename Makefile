.PHONY: build
build:
	@dune build

.PHONY: run
run: build
	@rlwrap dune exec rascal

.PHONY: watch
watch: build
	@dune runtest -w

.PHONY: test
test: build
	@dune runtest

.PHONY: install
install: build
	@sudo cp _build/install/default/bin/rascal /usr/local/bin
