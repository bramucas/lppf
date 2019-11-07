#!/bin/bash
TESTFILES=tests/test?
OPTIONS="-q -n 0 --verbose 4"

# Choosing difftool
if ! [ -x "$(command -v git)" ]; then
	# Git is not installed
	DIFFTOOL="diff"
	echo "Git is not installed. Using 'diff' command instead."
else
	DIFFTOOL="git diff --no-index --"
fi


for test in $TESTFILES 
do
	echo "Running ${test}..."
    ./lppf $OPTIONS $test | $DIFFTOOL "$test"_expected_result -
done

