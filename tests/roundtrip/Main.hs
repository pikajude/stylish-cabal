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
