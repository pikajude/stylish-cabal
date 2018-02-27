{-# Language NoMonomorphismRestriction #-}
{-# Language OverloadedStrings #-}

module Main where

import Data.Char
import Data.Monoid.Compat
import Options.Applicative hiding (ParserResult(..))
import Prelude.Compat
import StylishCabal
import System.Exit
import System.IO

data Opts = Opts
    { file :: Maybe FilePath
    , inPlace :: Bool
    , color :: Bool
    , width :: Int
    , renderOpts :: RenderOptions
    } deriving (Show)

renderopts :: Parser RenderOptions
renderopts =
    RenderOptions <$>
    option
        auto
        (long "indent" <> short 'n' <> help "Indent size in spaces" <> showDefault <>
         value 2 <>
         metavar "INT") <*>
    switch
        (long "simplify-versions" <> short 's' <>
         help "Simplify version ranges present in the Cabal file, if possible" <>
         showDefault)

opts :: Parser Opts
opts =
    Opts <$> optional (strArgument (metavar "FILE" <> help "Input file")) <*>
    switch (long "in-place" <> short 'i' <> help "Format file in place" <> showDefault) <*>
    flag
        True
        False
        (long "disable-color" <> short 'c' <>
         help
             "Disable colorized output (already disabled if using --in-place or if output is a file)") <*>
    option
        auto
        (long "width" <> short 'w' <> help "Character width limit for cabal file" <>
         showDefault <>
         value 90 <>
         metavar "INT") <*>
    renderopts

output :: Opts -> Doc -> Handle -> IO ()
output o doc h = do
    isTerminal <- hIsTerminalDevice h
    if color o && isTerminal
        then displayIO h (f doc)
        else do
            let docStr = unlines . map stripBlank . lines $ displayS (f $ plain doc) ""
            hPutStr h docStr
  where
    f = render (width o)
    stripBlank x
        | all isSpace x = []
        | otherwise = x

main :: IO ()
main = do
    o <- execParser $ info (opts <**> helper) (fullDesc <> progDesc "Format a Cabal file")
    f <- maybe getContents readFile (file o)
    doc <- prettyOpts (renderOpts o) <$> readPackageDescription (file o) f
    if inPlace o
        then case file o of
                 Just fname -> withFile fname WriteMode (output o doc)
                 Nothing -> hPutStrLn stderr inPlaceErr >> exitFailure
        else output o doc stdout
  where
    inPlaceErr = "stylish-cabal: --in-place specified, but I'm reading from stdin"
