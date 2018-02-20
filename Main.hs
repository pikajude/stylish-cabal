{-# Language CPP #-}
{-# Language NoMonomorphismRestriction #-}
{-# Language OverloadedStrings #-}

module Main where

import Data.Char
import Options.Applicative hiding (ParserResult(..))
import StylishCabal
import System.Exit
import System.IO

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif

data Opts = Opts
    { file :: Maybe FilePath
    , inPlace :: Bool
    , color :: Bool
    , width :: Int
    , indent :: Int
    } deriving (Show)

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
    option
        auto
        (long "indent" <> short 'n' <> help "Indent size in spaces" <> showDefault <>
         value 2 <>
         metavar "INT")

output :: Opts -> Doc -> Handle -> IO ()
output o doc h = do
    isTerminal <- hIsTerminalDevice stdout
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
    doc <- prettyWithIndent (indent o) <$> readCabalFile (file o) f
    if inPlace o
        then case file o of
                 Just fname -> withFile fname WriteMode (output o doc)
                 Nothing -> hPutStrLn stderr inPlaceErr >> exitFailure
        else output o doc stdout
  where
    inPlaceErr = "stylish-cabal: --in-place specified, but I'm reading from stdin"
