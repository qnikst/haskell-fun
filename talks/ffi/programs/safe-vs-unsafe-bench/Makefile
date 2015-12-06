all: bench bench-threaded

bench: bench.hs
	ghc -O2 $< -o $@

bench-threaded: bench.hs
	ghc -O2 -threaded $< -o $@
