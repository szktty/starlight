.PHONY: all clean setup compiler vm vm-debug

PROG=strl
PROG_RUN=strlrun

OCAML_PKG = core ppx_let yojson menhir


all: compiler vm

setup:
	opam install $(OCAML_PKG)

compiler:
	cd compiler; omake
	cp compiler/$(PROG) .

vm:
	cd vm; cargo build --release
	cp vm/target/release/$(PROG_RUN) .

vm-debug:
	cd vm; cargo build
	cp vm/target/debug/$(PROG_RUN) .

clean:
	cd compiler; omake clean
	cd vm; cargo clean
	rm -rf $(PROG) $(PROG_RUN)
