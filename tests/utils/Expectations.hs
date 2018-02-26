{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language StandaloneDeriving #-}

module Expectations where

import qualified Data.ByteString.UTF8 as U
import Data.List.Compat
import Distribution.PackageDescription.Parsec
import Prelude.Compat
import SortedPackageDescription
import StylishCabal as S
import Test.Hspec
import Test.Hspec.Core.Spec
import Test.Hspec.Core.Runner

hspecColor = hspecWith (defaultConfig {configColorMode = ColorAlways})

expectParse cabalStr = do
    let doc =
            U.fromString . (`displayS` "") . render 80 . plain . pretty <$>
            S.parsePackageDescription cabalStr
    case doc of
        S.Success rendered -> do
            let ([], Right original) = fmap sortGenericPackageDescription <$> parse' cabalStr
                ([], Right new) = fmap sortGenericPackageDescription <$> parse' rendered
            shouldBe original new
        Warn {} ->
            expectationFailure
                "SKIP Warnings generated from original file, cannot guarantee consistency of output"
        S.Error {} -> expectationFailure "SKIP Original cabal file does not parse"
  where
    parse' = runParseResult . parseGenericPackageDescription

applySkips i =
    i
        { itemExample =
              \a b c -> do
                  res <- itemExample i a b c
                  case res of
                      Right (Failure _ (Reason r))
                          | "SKIP " `isPrefixOf` r ->
                              pure $ Right $ Pending $ Just $ drop 5 r
                      x -> return x
        }

mkHeader p = "parses " ++ p
