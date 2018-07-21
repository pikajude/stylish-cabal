module Main where

import Data.String
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Parsec.ParseResult
import Distribution.Verbosity
import StylishCabal
import Test.HUnit
import Test.Hspec hiding (expectationFailure, shouldBe)
import Test.Hspec.Expectations.Pretty
import Text.PrettyPrint.ANSI.Leijen

testFile filepath =
    it ("parses " ++ filepath) $ do
        gpd1 <- readGenericPackageDescription normal filepath
        Right gpdDoc <- prettyPrintFile filepath
        case runParseResult $
             parseGenericPackageDescription (fromString $ displayS (plain' gpdDoc) "") of
            (_, Right y) -> y `shouldBe` gpd1
            x -> expectationFailure $ "no parse " ++ show x

main =
    hspec $ do
        describe "stylish-cabal" $ do
            testFile "stylish-cabal.cabal"
            testFile "../Cabal/cabal-install/cabal-install.cabal"
            testFile "../Cabal/Cabal/Cabal.cabal"

plain' (SSGR _ sd) = plain' sd
plain' (SLine n sd) = SLine n (plain' sd)
plain' (SText n t sd) = SText n t (plain' sd)
plain' (SChar c sd) = SChar c (plain' sd)
plain' x = x
