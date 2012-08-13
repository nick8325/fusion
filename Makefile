all: Church.hcr

%.hcr: %.hs Makefile
	ghc -c -O -ddump-simpl -dsuppress-module-prefixes -dsuppress-uniques -dsuppress-coercions $< > $@
