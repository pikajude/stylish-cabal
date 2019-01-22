{-# OPTIONS_GHC -fno-warn-orphans #-}

module Expectations where

import qualified Data.ByteString.UTF8          as U
import           Data.List.Compat
import           Distribution.PackageDescription.Parsec
import           Prelude.Compat
import           SortedPackageDescription
import           StylishCabal                  as S
import           Test.Hspec
import           Test.Hspec.Core.Runner

hspecColor = hspecWith (defaultConfig { configColorMode = ColorAlways })

expectParse cabalStr = do
  let doc =
        U.fromString
          .   (`displayS` "")
          .   render 80
          .   plain
          .   pretty
          <$> S.parsePackageDescription cabalStr
  case doc of
    S.Success rendered -> do
      let ([], Right original) =
            fmap sortGenericPackageDescription <$> parse' cabalStr
          ([], Right new) =
            fmap sortGenericPackageDescription <$> parse' rendered
      shouldBe original new
    Warn{} ->
      pendingWith
        "Warnings generated from original file, cannot guarantee consistency of output"
    S.Error{} -> pendingWith "Original cabal file does not parse"
  where parse' = runParseResult . parseGenericPackageDescription

mkHeader n p = "parses #" ++ show n ++ ": " ++ p
