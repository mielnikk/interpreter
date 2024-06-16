STACK = stack

all:
	${STACK} build 
	mv ${shell find . -name interpreter -type f | head -n 1} interpreter

clean:
	${STACK} clean
	rm interpreter