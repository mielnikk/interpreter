CABAL = cabal


all:
	${CABAL} build 
	mv dist/build/interpreter/interpreter interpreter


clean:
	${CABAL} clean
	rm interpreter