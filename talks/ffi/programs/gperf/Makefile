all: bench

bench: bench.hs table.o table.2.o
	ghc -g -O2 $^ -o $@

table.o: table.c
	gcc -c -O3 $^ -o $@

table.c: words-1000.txt
	gperf words-1000.txt > table.c

