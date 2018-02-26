module Main where

import Data.ByteString as B
import Expectations
import Prelude.Compat
import Test.Hspec

main :: IO ()
main =
    hspecColor $
    describe "comprehensive check" $ do
        it "retains every attribute" $
            expectParse =<< B.readFile "tests/cabal-files/example"
        it "codeblocks" $ expectParse =<< B.readFile "tests/cabal-files/hpc-coveralls"
