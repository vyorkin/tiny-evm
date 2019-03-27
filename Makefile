build:
	cabal new-build all

repl:
	cabal new-repl all

test:
	cabal new-test all

clean:
	cabal new-clean

.PHONY: build repl test clean

# to dump TH splices:
# --ghc-options="-ddump-splices"
