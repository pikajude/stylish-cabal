{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# Language OverloadedStrings #-}
{-# Language DeriveGeneric #-}

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Maybe
import qualified Data.Vector as V
import Expectations
import GHC.Generics
import Network.Wreq
import Prelude.Compat
import System.Environment
import System.IO
import System.Random.MWC
import System.Random.MWC.Distributions
import Test.Hspec
import Test.Hspec.Core.Spec
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

-- if we make a significant change to cabal file rendering, we want to
-- rerun on *all* of hackage, but the normal testsuite shouldn't do that
-- because it takes so long. set SKIP=0 to run on all packages
testHackage = do
    skip <- runIO $ (readMaybe =<<) <$> (lookupEnv "SKIP")
    packages <-
        runIO $ do
            hSetBuffering stdout NoBuffering
            putStrLn "getting package list..."
            packages' <- getJson "http://hackage.haskell.org/packages/"
            let packages = filter (not . isProblematic) packages'
            putStrLn "done, running tests..."
            if isNothing skip
                then createSystemRandom >>=
                     fmap (V.take 100) . uniformShuffle (V.fromList packages)
                else pure $ V.fromList packages
    parallel $
        describe "for 100 random Hackage packages" $
        forM_ (drop (fromMaybe 0 skip) $ zip [0 ..] $ V.toList packages) $ \(n, GetPackage pname) ->
            mapSpecItem_ applySkips $
            it (mkHeader n pname) $ do
                revs <-
                    getJson $
                    "http://hackage.haskell.org/package/" ++ pname ++ "/revisions/"
                let recent = last revs
                cabalFile <-
                    get $
                    "http://hackage.haskell.org/package/" ++
                    pname ++ "/revision/" ++ show (number recent) ++ ".cabal"
                expectParse $ toString $ view responseBody cabalFile

isProblematic _ = False

main :: IO ()
main = hspecColor $ describe "comprehensive check" testHackage
