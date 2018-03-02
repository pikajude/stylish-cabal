{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-signatures #-}

module Main where

import Expectations
import Prelude.Compat
import Test.Hspec

main :: IO ()
main =
    hspecColor $
    describe "comprehensive check" $ do
        it "retains every attribute" $
            expectParse =<< readFile "tests/cabal-files/example.txt"
        it "codeblocks" $ do
            expectParse =<< readFile "tests/cabal-files/hpc-coveralls.txt"
            expectParse =<< readFile "tests/cabal-files/helics.txt"
        it "non-ghc compilers" $
            expectParse =<< readFile "tests/cabal-files/exposed-containers.txt"
        it "baffling version constraints" $
            expectParse =<< readFile "tests/cabal-files/deka-tests.txt"
        it "markup" $ do
            expectParse =<< readFile "tests/cabal-files/BiGUL.txt"
            expectParse =<< readFile "tests/cabal-files/ChasingBottoms.txt"
            expectParse =<< readFile "tests/cabal-files/OrPatterns.txt"
            expectParse =<< readFile "tests/cabal-files/Cardinality.txt"
