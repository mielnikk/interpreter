STACK = stack

all:
	${STACK} build 
	mv .stack-work/dist/x86_64-linux/Cabal-3.4.1.0/build/interpreter/interpreter interpreter

clean:
	${STACK} clean
	rm interpreter