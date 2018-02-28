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
            expectParse =<< readFile "tests/cabal-files/example"
        it "codeblocks" $ expectParse =<< readFile "tests/cabal-files/hpc-coveralls"
        it "non-ghc compilers" $
            expectParse =<< readFile "tests/cabal-files/exposed-containers"
        it "baffling version constraints" $
            expectParse =<< readFile "tests/cabal-files/deka-tests"
