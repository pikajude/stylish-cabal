{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language StandaloneDeriving #-}

module Main where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy.UTF8 (toString)
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import Distribution.PackageDescription.Parse
import Distribution.ParseUtils (PWarning(..))
import GHC.Generics
import Network.Wreq
import Options.Applicative hiding (header)
import SortedDesc
import StylishCabal
import System.Environment
import System.IO
import Test.Hspec (describe, hspec, it, parallel, runIO)
import Test.Hspec.Core.Spec
import qualified Test.Hspec.Core.Spec as H
import Test.Hspec.Expectations.Pretty
import Text.PrettyPrint.ANSI.Leijen (displayS, plain, renderSmart)
import Text.Read (readMaybe)

newtype GetPackage = GetPackage
    { packageName :: String
    } deriving (Show, Generic)

data GetRevision = GetRevision
    { time :: String
    , user :: String
    , number :: Integer
    } deriving (Show, Generic)

instance FromJSON GetPackage

instance FromJSON GetRevision

getJson :: FromJSON b => String -> IO b
getJson x =
    fmap (view responseBody) $
    asJSON =<< getWith (defaults & header "Accept" .~ ["application/json"]) x

main :: IO ()
main = do
    v <- lookupEnv "SKIP"
    let skip = fromMaybe 0 (readMaybe =<< v)
    hspec $ do
        describe "comprehensive check" $ do
            it "retains every attribute" $ expectParse =<< readFile "tests/example.cabal"
            testHackage skip
#if TEST_HACKAGE
testHackage skip = do
    packages <-
        runIO $ do
            hSetBuffering stdout NoBuffering
            putStrLn "getting package list..."
            packages <- getJson "http://hackage.haskell.org/packages/"
            putStrLn "done, running tests..."
            return packages
    parallel $ do
        describe "for every Hackage package" $ do
            forM_ (drop skip $ zip [0 ..] packages) $ \(i, GetPackage pname) -> do
                unless (badPackage pname) $
                    mapSpecItem_ skipOldFiles $ do
                        it (mkHeader i pname) $ do
                            revs <-
                                getJson $
                                "http://hackage.haskell.org/package/" ++
                                pname ++ "/revisions/"
                            let recent = last revs
                            cabalFile <-
                                get $
                                "http://hackage.haskell.org/package/" ++
                                pname ++ "/revision/" ++ show (number recent) ++ ".cabal"
                            expectParse $ toString $ view responseBody cabalFile
#else
testHackage _ = pure ()
#endif
skipOldFiles i =
    i
        { itemExample =
              \a b c -> do
                  result <- itemExample i a b c
                  case result of
                      Right (H.Failure _ (H.Reason r))
                          | "SKIP " `isPrefixOf` r ->
                              pure $ Right $ H.Pending $ Just $ drop 5 r
                      x -> return x
        }

mkHeader i p = "parses #" ++ show i ++ ": " ++ p

expectParse cabalStr = do
    let doc = (`displayS` "") . renderSmart 1.0 80 . plain . pretty 2 <$> parse cabalStr
    case doc of
        StylishCabal.Success rendered -> do
            let original =
                    SortedDesc.from <$> parse' cabalStr :: ParseResult SGenericPackageDescription
                new = SortedDesc.from <$> parse' rendered
            shouldBe original new
        Warn {} ->
            expectationFailure
                "SKIP Warnings generated from original file, cannot guarantee consistency of output"
        StylishCabal.Error {} ->
            expectationFailure "SKIP Original cabal file does not parse"
  where
    parse' = parseGenericPackageDescription
    tooOld (ParseOk ws _) =
        any (\(PWarning s) -> "Unknown fields: build-depends" `isInfixOf` s) ws
    dropSomeWarnings (ParseOk pw a) = ParseOk (filter (not . fixedWarning) pw) a
      where
        fixedWarning (PWarning s) =
            or
                [ "must use section syntax" `isInfixOf` s
                -- edge case: very old Cabal versions don't use section
                -- syntax. stylish-cabal isn't going to generate old-style
                -- cabal files. the output still parses so we just ignore
                -- this case.
                , "must specify at least" `isInfixOf` s
                ]

deriving instance Eq a => Eq (ParseResult a)

badPackage :: String -> Bool
badPackage p = p `elem` S.fromList ["AppleScript", "DSTM", "cabal-plan"]

unwarn :: ParseResult a -> ParseResult a
unwarn (ParseOk _ x) = ParseOk [] x
unwarn x = x
