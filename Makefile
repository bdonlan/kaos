all: .setup-config
	runhaskell Setup.lhs build

.setup-config: Setup.lhs kaos.cabal
	runhaskell Setup.lhs configure

clean:
	runhaskell Setup.lhs clean
	rm -f kaos *.o *.hi *~

.PHONY: clean all
