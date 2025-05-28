
all: while_compiler.exe
	./while_compiler.exe --debug test.wl
	gcc -no-pie -g test.s && ./a.out

while_compiler.exe:
	dune build while_compiler.exe

clean:
	dune clean

.PHONY: all clean while_compiler.exe
