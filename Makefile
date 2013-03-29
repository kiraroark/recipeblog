all: clean
	ghc -Wall --make main.hs
clean:
	rm -f main.hi
	rm -f main.o
	rm -f main
	
