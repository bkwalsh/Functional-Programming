all:
	ghc Main.hs

clean:
	rm -f *hi *o Main

test:
	ghc Main.hs && ./Main < test_inputs.txt

test-diff:
	ghc Main.hs && ./Main < test_inputs.txt | diff -y test_expectation.txt -
