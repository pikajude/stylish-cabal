{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# Language OverloadedStrings #-}
{-# Language DeriveGeneric #-}

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy.UTF8 (toString)
import GHC.Generics
import Network.Wreq
import System.Random.MWC
import System.Random.MWC.Distributions
import System.IO
import Test.Hspec
import qualified Data.Vector as V
import Test.Hspec.Core.Spec
import Utils

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

testHackage = do
    packages <-
        runIO $ do
            hSetBuffering stdout NoBuffering
            putStrLn "getting package list..."
            packages <- getJson "http://hackage.haskell.org/packages/"
            putStrLn "done, running tests..."
            gen <- createSystemRandom
            uniformShuffle (V.fromList packages) gen
    parallel $
        describe "for 100 random Hackage packages" $
        forM_ (V.take 100 packages) $ \(GetPackage pname) ->
            mapSpecItem_ applySkips $
            it (mkHeader pname) $ do
                revs <-
                    getJson $
                    "http://hackage.haskell.org/package/" ++ pname ++ "/revisions/"
                let recent = last revs
                cabalFile <-
                    get $
                    "http://hackage.haskell.org/package/" ++
                    pname ++ "/revision/" ++ show (number recent) ++ ".cabal"
                expectParse $ toString $ view responseBody cabalFile

main :: IO ()
main = hspec $ describe "comprehensive check" testHackage