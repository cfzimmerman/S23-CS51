all: order_another order orderedcoll prioqueue tests

order_another: order_another.ml
	ocamlbuild -use-ocamlfind order_another.byte

order: order.ml
	ocamlbuild -use-ocamlfind order.byte

orderedcoll: orderedcoll.ml
	ocamlbuild -use-ocamlfind orderedcoll.byte

prioqueue: prioqueue.ml
	ocamlbuild -use-ocamlfind prioqueue.byte

tests: tests.ml 
	ocamlbuild -use-ocamlfind tests.byte