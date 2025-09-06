# --ghc 9.6.7 --cabal 3.12.1.0 son las versiones recommendades al momento de escribir este Makefile
GHC_VERSION=9.6.7
CABAL_VERSION=3.12.1.0
GHCUP_ENV=ghcup run --ghc $(GHC_VERSION) --cabal $(CABAL_VERSION)

.PHONY: deps
deps:
	$(GHCUP_ENV) -- cabal update

.PHONY: test
test:
	$(GHCUP_ENV) -- cabal test --test-show-details=direct

.PHONY: repl
repl:
	$(GHCUP_ENV) -- cabal repl 

bin/ghcid:
	$(GHCUP_ENV) -- cabal install ghcid --installdir=bin

.PHONY: watch
watch: bin/ghcid
	$(GHCUP_ENV) -- bin/ghcid --warnings --target=incierticalc-test --restart=incierticalc.cabal --test main

.PHONY: clean
clean:
	rm -rf dist-newstyle
	rm -rf bin