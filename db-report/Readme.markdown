# Utility to build information about use of the postgres database.


A small tool that connects to the database and gather differrent kind
of statistics, like table and index sizes, cache hit rate, index usage.

The tool is not yet complete as in this state it's enough for my needs
at work, but I'll continue working on that and slowly improving it.

1.  How to run:

```
nix-shell -p "haskellPackages.ghcWithPackages (p: [p.hasql p.lens ])"
```

In the shell run

```
export PG_HOST=postgresql://user:password@host:port/database
ghci report.hs -package hasql -package lens -package colonnade -package blaze-html -package blaze-markup -package vector -package text-show -package scientific -package foldl -Wall
```

Unfortunately blaze-colonnade and hasql-th do not build under nixos
of the specific version. But patches that will either fix nixpkgs
or add shell.nix will be kindly accepted.
