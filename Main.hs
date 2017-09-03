{-# Language NoMonomorphismRestriction #-}
{-# Language OverloadedStrings         #-}

module Main where

import Data.Monoid
import Options.Applicative
import StylishCabal
import System.IO

data Opts = Opts
          { file       :: FilePath
          , inPlace    :: Bool
          , formatOpts :: FormatOpts
          } deriving Show

opts :: Parser Opts
opts = Opts
    <$> strArgument (metavar "FILE" <> help "Input file")
    <*> switch
        ( long "in-place"
       <> short 'i'
       <> help "Format file in place"
       <> showDefault )
    <*> (FormatOpts
        <$> option auto
            ( long "width"
           <> short 'w'
           <> help "Character width limit for cabal file"
           <> showDefault
           <> value 90
           <> metavar "INT")
        <*> option auto
            ( long "indent"
           <> short 'n'
           <> help "Indent size in spaces"
           <> showDefault
           <> value 2
           <> metavar "INT"))

main :: IO ()
main = do
    o <- execParser $ info (opts <**> helper) (fullDesc <> progDesc "Format a Cabal file")

    f <- readFile (file o)

    newFile <- prettify f (formatOpts o)

    if inPlace o
        then writeFile (file o) newFile
        else putStrLn newFile
