BUILD = ocamlbuild -use-ocamlfind 

all: nativeLazyStreams music refs \
	sampleStreams streamstrees passert \
	refs_test streams_trees_tests \
	music_tests

nativeLazyStreams: nativeLazyStreams.ml 
	${BUILD} nativeLazyStreams.byte

music: music.ml
	${BUILD} music.byte 

refs: refs.ml 
	${BUILD} refs.byte 

sampleStreams: sampleStreams.ml 
	${BUILD} sampleStreams.byte 

streamstrees: streamstrees.ml 
	${BUILD} streamstrees.byte

passert: passert.ml 
	${BUILD} passert.byte

refs_test: refs_test.ml 
	${BUILD} refs_test.byte

streams_trees_tests: streams_trees_tests.ml 
	${BUILD} streams_trees_tests.byte

music_tests: music_tests.ml 
	${BUILD} music_tests.byte