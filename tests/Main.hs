{-# Language DeriveGeneric       #-}
{-# Language OverloadedStrings   #-}
{-# Language RecordWildCards     #-}
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
import           Data.Data.Lens
import           Data.List                             (sort)
import qualified Data.Set                              as S
import           Distribution.PackageDescription.Parse
import           GHC.Generics
import           Network.Wreq
import           SortedDesc
import           StylishCabal
import           System.IO
import           System.IO.Temp
import           Test.Hspec                            (hspec, it)
import           Test.Hspec.Expectations.Pretty
import           Test.HUnit

data GetPackage = GetPackage { packageName :: String }
                deriving (Show, Generic)

data GetRevision = GetRevision
                 { time   :: String
                 , user   :: String
                 , number :: Integer
                 }
                 deriving (Show, Generic)

instance FromJSON GetPackage
instance FromJSON GetRevision

getJson x = fmap (view responseBody)
    $ asJSON =<< getWith (defaults & header "Accept" .~ ["application/json"]) x

skip = 11242

main = hspec $ it "parse" $ do
    packages <- getJson "http://hackage.haskell.org/packages/"
    qs <- newQSem 10
    mv <- newMVar 0
    let showLatest = do
            i <- readMVar mv
            putStrLn $ "New skip number: " ++ show i
    flip finally showLatest $ forConcurrently_ (drop skip $ zip [0..] packages) $
        \ (i, GetPackage pname) -> unless (badPackage pname) $ do
            waitQSem qs
            revs <- getJson $ "http://hackage.haskell.org/package/" ++ pname ++ "/revisions/"
            let recent = last revs
            cabalFile <- get $ "http://hackage.haskell.org/package/" ++ pname ++ "/revision/" ++ show (number recent) ++ ".cabal"
            let cabalStr = toString $ view responseBody cabalFile
            (do
                p <- prettify cabalStr 100
                shouldBe
                    (unwarn $ SortedDesc.from <$> parsePackageDescription cabalStr :: ParseResult SGenericPackageDescription)
                    (unwarn $ SortedDesc.from <$> parsePackageDescription p)
                modifyMVar_ mv (\ x -> return $ max x i)
                hFlush stdout
                signalQSem qs)
                `onException` (putStrLn $ "Testing #" ++ show i ++ " (" ++ pname ++ ") failed")

deriving instance Eq a => Eq (ParseResult a)

badPackage p = p `elem` S.fromList ["AppleScript", "cabal-plan"]

unwarn (ParseOk _ x) = ParseOk [] x
unwarn x             = x
