#!/bin/bash

# Config
TESTFILES=tests/test?
OPTIONS="-q -n 0 --format cterms --labelHeads all"

### Managing options
# By default
FORCEDIFF=false
# Processing options
while test $# -gt 0; do
  case "$1" in
    -h|--help)
      echo "$0 - Automatic tests for lppf"
      echo " "
      echo "Usage: $0 [--stdiff]"
      echo "Options:"
      echo "   --stdiff     Forces $0 to use the standard 'diff' command as diff tool "
      echo "                instead of using 'git diff' which is the default. If git is not"
      echo "                installed then $0 will switch to 'diff' automatically."
      exit 0
      ;;
    --stdiff)
      shift
      	FORCEDIFF=true
      ;;
    *)
      break
      ;;
  esac
done

# Choosing difftool
if ( [ -x "$(command -v git)" ]) | $FORCEDIFF; then  # Git is not installed or --stdiff flag is active.
	if $FORCEDIFF = true; then
		message="Using 'diff' command."
	else
		message="Git is not installed. Using 'diff' command instead."
	fi

	DIFFTOOL="diff"
	echo $message
else
	DIFFTOOL="git diff --no-index --"
fi

# Running tests
for test in $TESTFILES 
do
	echo "Running ${test}..."
    ./lppf $OPTIONS $test | $DIFFTOOL "$test"_expected_result -
done

