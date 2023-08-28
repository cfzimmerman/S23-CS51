all: native sec6 


native: nativeLazyStreams.ml
	ocamlbuild -use-ocamlfind nativeLazyStreams.byte

sec6: section6.ml
	ocamlbuild -use-ocamlfind section6.byte







clean:
	rm -rf _build *.byte