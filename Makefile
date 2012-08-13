all: Church.hcr

%.hcr: %.hs
	ghc -c -O -ddump-simpl -dsuppress-module-prefixes -dsuppress-uniques -dsuppress-coercions $< > $@
