STACK = stack

all:
	${STACK} build 
	mv $(find . -name interpreter -type f) interpreter

clean:
	${STACK} clean
	rm interpreter