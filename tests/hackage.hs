{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Codec.Archive.Tar as Tar
import Control.Exception (throwIO)
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.UTF8 (fromString)
import Data.Foldable
import Data.Functor.Identity
import Data.List (isPrefixOf, isSuffixOf)
import Data.TreeDiff.Class
import Data.TreeDiff.Pretty
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Parsec.ParseResult
import Distribution.Verbosity
import StylishCabal
import System.Directory
import System.Exit (die)
import System.FilePath ((</>))
import System.IO
import Test.HUnit
import Test.Hspec
import Test.Hspec.Core (ResultStatus(..))
import Text.PrettyPrint.ANSI.Leijen (SimpleDoc(..), displayS)

import Instances.TreeDiff ()

hackageIndex = do
    cabalDir <- getAppUserDataDirectory "cabal"
    let tarfile = cabalDir </> "packages" </> "hackage.haskell.org" </> "01-index.tar"
    doesFileExist tarfile >>= \case
        True -> do
            bytes <- LB.readFile tarfile
            return $ Tar.foldEntries cons' [] (error . show) $ Tar.read bytes
        False -> die $ "Hackage index file " ++ show tarfile ++ " does not exist"
  where
    cons' ent entries =
        case Tar.entryContent ent of
            Tar.NormalFile bs _
                | ".cabal" `isSuffixOf` path && "c" `isPrefixOf` path ->
                    (path, LB.toStrict bs) : entries
            _ -> entries
      where
        path = Tar.entryPath ent

testFile (fpath, bytes) =
    it ("roundtrips " ++ fpath) $ do
        gpd1 <- parse "first pass" bytes
        case runIdentity $ prettyPrintBytes bytes of
            Right sd -> do
                let prettyBytes = displayS (plain' sd) ""
                gpd2 <- parse "second pass" (fromString prettyBytes)
                let pd1 = packageDescription gpd1
                    pd2 = packageDescription gpd2
                unless (pd1 == pd2) $
                    assertFailure
                        (show (ansiWlEditExpr $ ediff pd1 pd2))
            Left x -> pendingWith (show x)
  where
    parse phase c = do
        let (_, x') = runParseResult $ parseGenericPackageDescription c
        case x' of
            Right gpd -> pure gpd
            Left (_, errs) -> do
                if phase == "first pass"
                    then throwIO (Pending Nothing $ Just "original file does not parse")
                    else assertFailure (unlines $ phase : map show errs)

main = do
    index <- hackageIndex
    hspec $ describe "stylish-cabal" $ mapM_ testFile index

plain' (SSGR _ sd) = plain' sd
plain' (SLine n sd) = SLine n (plain' sd)
plain' (SText n t sd) = SText n t (plain' sd)
plain' (SChar c sd) = SChar c (plain' sd)
plain' x = x
