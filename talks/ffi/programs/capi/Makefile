all: test

test: test.hs pi.o pi.h
	ghc test.hs pi.o -keep-temp

pi.o: pi.c pi.h
	$(CC) $< -c -o $@
