module Main where

import qualified Data.ByteString as B
import Expectations
import Prelude.Compat
import Test.Hspec

main :: IO ()
main =
    hspecColor $
    describe "comprehensive check" $ do
        it "retains every attribute" $
            expectParse =<< B.readFile "tests/cabal-files/example.txt"
        it "codeblocks" $ do
            expectParse =<< B.readFile "tests/cabal-files/hpc-coveralls.txt"
            expectParse =<< B.readFile "tests/cabal-files/helics.txt"
        it "non-ghc compilers" $
            expectParse =<< B.readFile "tests/cabal-files/exposed-containers.txt"
        it "baffling version constraints" $
            expectParse =<< B.readFile "tests/cabal-files/deka-tests.txt"
        it "markup" $ do
            expectParse =<< B.readFile "tests/cabal-files/BiGUL.txt"
            expectParse =<< B.readFile "tests/cabal-files/ChasingBottoms.txt"
            expectParse =<< B.readFile "tests/cabal-files/OrPatterns.txt"
            expectParse =<< B.readFile "tests/cabal-files/Cardinality.txt"
