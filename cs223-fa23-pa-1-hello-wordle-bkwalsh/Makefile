
# NOTE: Setting SHELL to `zsh` (rather than the default `sh`) for its
# nicer input redirection and command substitution, used in test-2
# through test-9. If you don't have `zsh`, comment out this line, and
# either create explicit intermediate files (see test-1) or search
# --- chat with your trusty AI? --- for more ways to mix commands
# and input redirection than I (Ravi) care to learn right now.

SHELL=/bin/zsh

all:
	ghc -o wordle Wordle.hs

clean:
	rm *hi *o wordle

tests: test-1 test-2 test-3 test-4 test-5 test-6 test-7 test-8 test-9

test-1:
	cat testing/input-1.txt | ./wordle 0 > testing/output-1.txt
	diff testing/output-1.txt testing/correct-output-1.txt

test-2:
	diff <(cat testing/input-2.txt | ./wordle 1898) testing/correct-output-2.txt

test-3:
	diff <(cat testing/input-3.txt | ./wordle 748) testing/correct-output-3.txt

test-4:
	diff <(./wordle --how-to-play) testing/how-to-play.txt

test-5:
	diff <(./wordle zero 2>&1) testing/invalid-game-number.txt

test-6:
	diff <(./wordle 123456789 2>&1) testing/invalid-game-number.txt

test-7:
	diff <(./wordle 2107 2>&1) testing/curry.txt

test-8:
	diff <(./wordle 0 –how-to-play 2>&1) testing/usage.txt

test-9:
	diff <(./wordle 1 1 2 3 5 2>&1) testing/usage.txt
