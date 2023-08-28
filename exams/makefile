all: notes notes.ml scratch_paper scratch_paper.ml

notes: notes.ml
	ocamlbuild -use-ocamlfind notes.byte

scratch_paper: scratch_paper.ml
	ocamlbuild -use-ocamlfind scratch_paper.byte

clean:
	rm -rf _build *.byte