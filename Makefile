all: Church.hcr

%.hcr: %.hs
	ghc -c -O -ddump-simpl $< > $@
