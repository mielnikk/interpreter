STACK = stack

all:
	${STACK} build 
	mv ${shell find .stack-work -name interpreter -type f | head -n 1} interpreter

clean:
	${STACK} clean
	rm interpreter