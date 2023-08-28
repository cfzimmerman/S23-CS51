all: collections draw mazes puzzledescription puzzlesolve tests tiles experiments

collections: collections.ml
	ocamlbuild -use-ocamlfind collections.byte

draw: draw.ml
	ocamlbuild -use-ocamlfind draw.byte

mazes: mazes.ml
	ocamlbuild -use-ocamlfind mazes.byte

puzzledescription: puzzledescription.ml
	ocamlbuild -use-ocamlfind puzzledescription.byte

puzzlesolve: puzzlesolve.ml 
	ocamlbuild -use-ocamlfind puzzlesolve.byte

tests: tests.ml 
	ocamlbuild -use-ocamlfind tests.byte

tiles: puzzlesolve.ml 
	ocamlbuild -use-ocamlfind tiles.byte

extra_tests: extra_tests.ml 
	ocamlbuild -use-ocamlfind extra_tests.byte

passert: passert.ml 
	ocamlbuild -use-ocamlfind passert.byte

experiments: experiments.ml
	ocamlbuild -use-ocamlfind experiments.byte