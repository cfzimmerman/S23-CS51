BUILD = ocamlbuild -use-ocamlfind 

all: config counter people registry run \
	simulation statistics utilities visualization \
	passert tests

config: config.ml 
	${BUILD} config.byte

counter: counter.ml 
	${BUILD} counter.byte

passert: passert.ml 
	${BUILD} passert.byte

people: people.ml 
	${BUILD} people.byte

registry: registry.ml 
	${BUILD} registry.byte

run: run.ml 
	${BUILD} run.byte

simulation: simulation.ml 
	${BUILD} simulation.byte

statistics: statistics.ml 
	${BUILD} statistics.byte

tests: tests.ml 
	${BUILD} tests.byte

utilities: utilities.ml 
	${BUILD} utilities.byte

visualization: visualization.ml 
	${BUILD} visualization.byte