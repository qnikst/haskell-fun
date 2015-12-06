test: test.hs bogus.o
	ghc --make -O2 $^

bogus.o: bogus.c
	$(CC) $^ -c -o $@
