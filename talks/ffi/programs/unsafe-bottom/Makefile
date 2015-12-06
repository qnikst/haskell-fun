runit: runit.hs bottom.o
	ghc $^ -o runit -threaded

bottom.o: bottom.c bottom.h
	$(CC) $< -c -o $@


