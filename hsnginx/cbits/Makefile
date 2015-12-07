GHC=ghc
NGNX_PATH=../vendor/nginx-1.9.6/
NGNX_INCLUDES=-I$(NGNX_PATH)/src/ -I$(NGNX_PATH)/src/core -I$(NGNX_PATH)/src/event -I$(NGNX_PATH)/src/http -I$(NGNX_PATH)/src/os/unix -I$(NGNX_PATH)/objs -I$(NGNX_PATH)/src/http/modules

all: My.o ngx_http_haskell_module.o test.o


test.o: My.o ngx_http_haskell_module.o
	ld -r -o test.o My.o ngx_http_haskell_module.o \
		/usr/lib64/ghc-7.10.2/rts/libHSrts.a \
		/usr/lib64/ghc-7.10.2/base_GDytRqRVSUX7zckgKqJjgw/libHSbase-4.8.1.0-GDytRqRVSUX7zckgKqJjgw.a \
		/usr/lib64/ghc-7.10.2/ghcpr_8TmvWUcS1U1IKHT0levwg3/libHSghc-prim-0.4.0.0-8TmvWUcS1U1IKHT0levwg3.a \
		/usr/lib64/ghc-7.10.2/integ_2aU3IZNMF9a7mQ0OzsZ0dS/libHSinteger-gmp-1.0.0.0-2aU3IZNMF9a7mQ0OzsZ0dS.a \

#-lpthread -ldl -lrt -lHSrts -lffi -L/usr/lib64/ghc-7.10.2/rts/ 


My.o: My.hs
	$(GHC) -c -static $< $(NGNX_INCLUDES)

ngx_http_haskell_module.o:  ngx_http_haskell_module.c My.o
	$(GHC) -c -static $< My.o $(NGNX_INCLUDES)
