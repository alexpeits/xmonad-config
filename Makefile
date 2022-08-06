.PHONY: test

ORMOLU_ARGS=-o -XTypeApplications -o -XInstanceSigs -o -XPatternSynonyms

format:
	ormolu ${ORMOLU_ARGS} -i $$(find lib/ -name '*.hs')
	ormolu ${ORMOLU_ARGS} -i xmonad.hs

