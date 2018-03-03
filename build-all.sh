#!/bin/bash

set -e

if [ "$#" -eq "0" ]; then
  compilers=(ghc742 ghc763 ghc784 ghc7103 ghc802 ghc822)
else
  compilers=($@)
fi

for compiler in ${compilers[@]}; do
    if [ "x$TEST_HACKAGE" != "x" ]; then
        test_hackage="-ftest-hackage"
    fi
    echo "Testing $compiler"
    nix-shell -p "haskell.compiler.$compiler" zlib --command "cabal new-build -fwerror && cabal new-test $test_hackage -fwerror"
done
