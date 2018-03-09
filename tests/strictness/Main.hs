{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# Language FlexibleContexts #-}

import Control.DeepSeq
import qualified Data.ByteString as B
import Distribution.PackageDescription
import qualified Distribution.PackageDescription as C
import Distribution.PackageDescription.Parsec
import Instances ()
import Prelude.Compat
import Pretty
import StylishCabal
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty
import Test.StrictCheck.Demands
import Test.StrictCheck.Instances ()
import Test.StrictCheck.Observe

whnfContext = flip seq ()

process gpd =
    deepseq
        ( C.library pd
        , C.subLibraries pd
        , C.executables pd
        , C.buildDepends pd
        , C.foreignLibs pd
        , C.testSuites pd
        , C.benchmarks pd)
        (length $ show $ pretty gpd)
  where
    pd = packageDescription gpd

main :: IO ()
main =
    hspec $ do
        ([], Right gpd) <-
            runIO $
            runParseResult . parseGenericPackageDescription <$>
            B.readFile "tests/cabal-files/example.txt"
        describe "pretty" $
            it "should fully evaluate the package description" $ do
                let expected = nf gpd
                    (_, actual) = observe1 whnfContext process gpd
                pprint actual `shouldBe` pprint expected
