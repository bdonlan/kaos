GHCOPTS+=-fglasgow-exts -fno-monomorphism-restriction -fallow-undecidable-instances

kaos:
	mkdir -p build/normal
	ghc --make $(GHCOPTS) -o kaos --make Main.hs \
		-odir build/normal

optimized:
	mkdir -p build/opt
	ghc --make $(GHCOPTS) -o kaos.opt --make Main.hs -O \
		-odir build/opt

profile:
	mkdir -p build/prof
	ghc --make $(GHCOPTS) -o kaos.prof --make Main.hs -prof \
		-odir build/prof

profopt:
	mkdir -p build/profopt
	ghc --make $(GHCOPTS) -o kaos --make Main.hs -O -prof \
		-odir build/profopt

all: kaos optimized profile profopt

clean:
	rm -f kaos *.o *.hi *~
	rm -rf build

.PHONY: kaos all optimized profile profopt clean
