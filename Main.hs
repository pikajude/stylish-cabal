{-# Language NoMonomorphismRestriction #-}
{-# Language OverloadedStrings         #-}
{-# Language RecordWildCards           #-}

module Main where

import Data.Monoid
import Options.Applicative
import StylishCabal
import System.IO

data Opts = Opts
          { file  :: FilePath
          , width :: Int
          } deriving Show

opts :: Parser Opts
opts = Opts
    <$> strArgument (metavar "FILE" <> help "Input file")
    <*> option auto
        ( long "width"
       <> short 'w'
       <> help "Character width limit for cabal file"
       <> showDefault
       <> value 90
       <> metavar "INT")

main :: IO ()
main = do
    o <- execParser $ info (opts <**> helper) (fullDesc <> progDesc "Format a Cabal file")

    withFile (file o) ReadMode $ \ h -> do
        f <- hGetContents h
        putStrLn =<< prettify f (Main.width o)
