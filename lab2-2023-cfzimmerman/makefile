all: lab2 tests

lab2: lab2.ml
	ocamlbuild -use-ocamlfind lab2.byte

lab2_tests: lab2_tests.ml
	ocamlbuild -use-ocamlfind lab2_tests.byte	

tests: lab2_tests
	./lab2_tests.byte

clean:
	rm -rf _build *.byte
