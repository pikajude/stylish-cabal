{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language DeriveGeneric       #-}
{-# Language OverloadedStrings   #-}
{-# Language ScopedTypeVariables #-}
{-# Language StandaloneDeriving  #-}

module Main where

import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Concurrent.QSem
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.ByteString.Lazy.UTF8             (toString)
import qualified Data.Set                              as S
import           Distribution.PackageDescription.Parse
import           GHC.Generics
import           Network.Wreq
import           SortedDesc
import           StylishCabal
import           Test.Hspec                            (hspec, it)
import           Test.Hspec.Expectations.Pretty

newtype GetPackage = GetPackage { packageName :: String }
                   deriving (Show, Generic)

data GetRevision = GetRevision
                 { time   :: String
                 , user   :: String
                 , number :: Integer
                 } deriving (Show, Generic)

instance FromJSON GetPackage
instance FromJSON GetRevision

getJson :: FromJSON b => String -> IO b
getJson x = fmap (view responseBody)
    $ asJSON =<< getWith (defaults & header "Accept" .~ ["application/json"]) x

main :: IO ()
main = hspec $ it "brute-force" $ do
    packages <- getJson "http://hackage.haskell.org/packages/"
    qs <- newQSem (8 :: Int)
    mv <- newMVar (0 :: Int)
    let showLatest = do
            i <- readMVar mv
            putStrLn $ "New skip number: " ++ show i
    flip finally showLatest $ forConcurrently_ (zip [0..] packages) $ \ (i, GetPackage pname) -> do
        waitQSem qs
        revs <- getJson $ "http://hackage.haskell.org/package/" ++ pname ++ "/revisions/"
        let recent = last revs
        cabalFile <- get $ "http://hackage.haskell.org/package/" ++ pname ++ "/revision/" ++ show (number recent) ++ ".cabal"
        let cabalStr = toString $ view responseBody cabalFile
        (do
            p <- prettify cabalStr $ FormatOpts 100 2
            -- parsing the original package description should yield the
            -- same result as parsing the formatted one
            shouldBe
                (unwarn $ SortedDesc.from <$> parseGenericPackageDescription cabalStr :: ParseResult SGenericPackageDescription)
                (unwarn $ SortedDesc.from <$> parseGenericPackageDescription p)
            modifyMVar_ mv (\ x -> return $ max x i)
            hFlush stdout
            signalQSem qs)
            `onException` putStrLn ("Testing #" ++ show i ++ " (" ++ pname ++ ") failed")

deriving instance Eq a => Eq (ParseResult a)

badPackage :: String -> Bool
badPackage p = p `elem` S.fromList ["AppleScript", "cabal-plan"]

unwarn :: ParseResult a -> ParseResult a
unwarn (ParseOk _ x) = ParseOk [] x
unwarn x             = x
