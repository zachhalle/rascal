build:
	@dune build

run: build
	@rlwrap dune exec rascal

watch: build
	@dune runtest -w

test: build
	@dune runtest
