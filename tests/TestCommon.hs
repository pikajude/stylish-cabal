module TestCommon where

import Control.Exception (throwIO)
import Control.Monad
import Data.ByteString.UTF8 (fromString)
import Data.Functor.Identity
import Data.TreeDiff.Class
import Data.TreeDiff.Pretty
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import StylishCabal
import Test.HUnit
import Test.Hspec
import Test.Hspec.Core (ResultStatus(..))
import Text.PrettyPrint.ANSI.Leijen (SimpleDoc(..), displayS)

import Instances.TreeDiff ()

testFile (fpath, bytes) =
    it ("roundtrips " ++ fpath) $ do
        gpd1 <- parse "first pass" bytes
        case runIdentity $ prettyPrintBytes bytes of
            Right sd -> do
                let prettyBytes = displayS (plain' sd) ""
                gpd2 <- parse "second pass" (fromString prettyBytes)
                let pd1 = packageDescription gpd1
                    pd2 = packageDescription gpd2
                unless (pd1 == pd2) $
                    assertFailure (show (ansiWlEditExpr $ ediff pd1 pd2))
            Left x -> pendingWith (show x)
  where
    parse phase c = do
        let (_, x') = runParseResult $ parseGenericPackageDescription c
        case x' of
            Right gpd -> pure gpd
            Left (_, errs) -> do
                if phase == "first pass"
                    then throwIO (Pending Nothing $ Just "original file does not parse")
                    else assertFailure (unlines $ phase : map show errs)

plain' (SSGR _ sd) = plain' sd
plain' (SLine n sd) = SLine n (plain' sd)
plain' (SText n t sd) = SText n t (plain' sd)
plain' (SChar c sd) = SChar c (plain' sd)
plain' x = x
