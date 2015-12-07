Attempt to try to write modules for NGINX using haskell

This work is far from completions. Following things are 
required at least:
  1. Implement bindings to more nginx functions
  2. Automate building process

How to build:
  1. bootstrap project using utils/bootstrap.sh that will download
     nginx and try to run bootstrap make
  2. Buiding will fail because nginx will be built with cc, not ghc.
  3. go to cbits folder and run Makefile in order to build haskell
     project
  4. copy .o files to relevant place in vendor/nginx-\*/objs/ and
     try to build nginx again.
