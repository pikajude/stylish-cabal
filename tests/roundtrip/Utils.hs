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
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure)
import Data.Functor ((<$>))
#endif
deriving instance Eq a => Eq (ParseResult a)

expectParse cabalStr = do
    let doc =
            (`displayS` "") . render 80 . plain . pretty <$>
            parseCabalFile cabalStr
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
                  res <- itemExample i a b c
                  case res of
                      Right (Failure _ (Reason r))
                          | "SKIP " `isPrefixOf` r ->
                              pure $ Right $ Pending $ Just $ drop 5 r
                      x -> return x
        }

mkHeader p = "parses " ++ p
