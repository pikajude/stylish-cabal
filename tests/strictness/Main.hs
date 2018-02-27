{-# Language FlexibleContexts #-}

import Control.DeepSeq
import Data.Word
import Distribution.License
import Distribution.PackageDescription
import qualified Distribution.PackageDescription as C
import Distribution.PackageDescription.Parse
import Distribution.Version
import qualified GHC.Generics as GHC
import Generics.SOP
import Generics.SOP.TH
import Instances
import Prelude.Compat
import Pretty
import StylishCabal
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty
import Test.StrictCheck.Demands
import Test.StrictCheck.Instances
import Test.StrictCheck.Instances.Tools
import Test.StrictCheck.Observe
import Test.StrictCheck.Shaped

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
        ParseOk _ gpd <-
            runIO $
            parseGenericPackageDescription <$> readFile "tests/cabal-files/example"
        describe "pretty" $
            it "should fully evaluate the package description" $ do
                let expected = nf gpd
                    (_, actual) = observe1 whnfContext process gpd
                pprint actual `shouldBe` pprint expected
