#!/bin/bash

set -e

for compiler in ghc763 ghc784 ghc7103 ghc802 ghc822; do
    if [ "$compiler" = "ghc784" -o "$compiler" = "ghc763" ]; then
        test_hackage=
    else
        test_hackage="-ftest-hackage"
    fi
    echo "Testing $compiler"
    nix-shell -p "haskell.compiler.$compiler" zlib --command "cabal new-build -fwerror && cabal new-test $test_hackage -fwerror"
done
