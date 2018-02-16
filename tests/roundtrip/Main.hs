{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-signatures #-}

module Main where

import Test.Hspec
import Utils

main :: IO ()
main =
    hspec $
    describe "comprehensive check" $
    it "retains every attribute" $ expectParse =<< readFile "tests/example.cabal"
