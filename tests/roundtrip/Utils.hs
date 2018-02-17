{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language CPP #-}
{-# Language StandaloneDeriving #-}

module Utils where

import Control.Monad
import Data.List
import Distribution.PackageDescription.Parse
import Distribution.ParseUtils (PWarning(..))
import SortedDesc
import StylishCabal
import Test.Hspec.Core.Spec
import Test.Hspec.Expectations.Pretty
import Text.PrettyPrint.ANSI.Leijen (displayS, plain, renderSmart)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (pure)
import Data.Functor ((<$>))
#endif

deriving instance Eq a => Eq (ParseResult a)

expectParse cabalStr = do
    let doc = (`displayS` "") . renderSmart 1.0 80 . plain . pretty 2 <$> parse cabalStr
    case doc of
        StylishCabal.Success rendered -> do
            let original =
                    from <$> parse' cabalStr :: ParseResult SGenericPackageDescription
                new = from <$> parse' rendered
            case new of
                ParseOk pws _ -> skipIfRedFlags pws
                _ -> pure ()
            shouldBe original new
        Warn {} ->
            expectationFailure
                "SKIP Warnings generated from original file, cannot guarantee consistency of output"
        StylishCabal.Error {} ->
            expectationFailure "SKIP Original cabal file does not parse"
  where
    skipIfRedFlags [] = return ()
    skipIfRedFlags pws =
        when (any (\(PWarning p) -> "must specify at least" `isInfixOf` p) pws) $
        expectationFailure
            "SKIP Original specified cabal-version is too old for section syntax"
    parse' = parseGenericPackageDescription

applySkips i =
    i
        { itemExample =
              \a b c -> do
                  result <- itemExample i a b c
                  case result of
                      Right (Failure _ (Reason r))
                          | "SKIP " `isPrefixOf` r ->
                              pure $ Right $ Pending $ Just $ drop 5 r
                      x -> return x
        }

mkHeader p = "parses " ++ p
