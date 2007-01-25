all: .setup-config
	runhaskell Setup.lhs build

.setup-config: Setup.lhs kaos.cabal
	runhaskell Setup.lhs configure

clean:
	[ -e .setup-config ] && runhaskell Setup.lhs clean
	rm -rf dist
	rm -f kaos *.o *.hi *~

.PHONY: clean all
