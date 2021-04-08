build:
	bnfc --haskell -o src Lambda.bnfc
	rm src/TestLambda.hs
	stack build
	
