{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language StandaloneDeriving #-}

module Utils where

import Data.List.Compat
import Distribution.PackageDescription.Parse
import Prelude.Compat
import SortedDesc
import StylishCabal as S
import Test.Hspec.Core.Spec
import Test.Hspec.Expectations.Pretty

deriving instance Eq a => Eq (ParseResult a)

expectParse cabalStr = do
    let doc =
            (`displayS` "") . render 80 . plain . pretty <$>
            S.parsePackageDescription cabalStr
    case doc of
        S.Success rendered -> do
            let original =
                    from <$> parse' cabalStr :: ParseResult SGenericPackageDescription
                new = from <$> parse' rendered
            shouldBe original new
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

mkHeader p = "parses " ++ p
