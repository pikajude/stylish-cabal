{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language StandaloneDeriving #-}

module Expectations where

import Control.Monad
import Data.List.Compat
import Distribution.PackageDescription.Parse
import Prelude.Compat
import SortedPackageDescription
import StylishCabal as S
import Test.Hspec
import Test.Hspec.Core.Runner
import Test.Hspec.Core.Spec
-- import Test.Hspec.Expectations.Pretty

deriving instance Eq a => Eq (ParseResult a)

hspecColor = hspecWith (defaultConfig {configColorMode = ColorAlways})

expectParse cabalStr = do
    -- massive width limit is because:
    --
    -- edge case: a line ending in " ." will be word-wrapped to the next
    -- line. unfortunately a "." on its own on a line means paragraph break
    -- to the haddock parser. this case breaks tests on some hackage
    -- packages, but is unlikely to be problematic in practice (because you
    -- should just remove the leading space)
    let proc = (`displayS` "") . render (maxBound `div` 10) . plain
        doc = proc . pretty <$> S.parsePackageDescription cabalStr
    case doc of
        S.Success rendered -> do
            let ParseOk _ (descrs, original) =
                    sortGenericPackageDescription <$> parse' cabalStr
                ParseOk _ (descrs2, new) =
                    sortGenericPackageDescription <$> parse' rendered
            shouldBe original new
            forM_ (zip descrs descrs2) (uncurry shouldBe)
        Warn {} ->
            expectationFailure
                "SKIP Warnings generated from original file, cannot guarantee consistency of output"
        S.Error {} -> expectationFailure "SKIP Original cabal file does not parse"
  where
    parse' = parseGenericPackageDescription

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

mkHeader n p = "parses #" ++ show n ++ ": " ++ p
