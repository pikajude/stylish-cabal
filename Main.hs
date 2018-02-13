{-# Language NoMonomorphismRestriction #-}
{-# Language OverloadedStrings #-}

module Main where

import Data.Monoid
import Options.Applicative
import StylishCabal
import System.Exit
import System.IO
import Text.PrettyPrint.ANSI.Leijen (displayIO)

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
         value 80 <>
         metavar "INT") <*>
    option
        auto
        (long "indent" <> short 'n' <> help "Indent size in spaces" <> showDefault <>
         value 2 <>
         metavar "INT")

main :: IO ()
main = do
    o' <-
        execParser $ info (opts <**> helper) (fullDesc <> progDesc "Format a Cabal file")
    isTerminal <- hIsTerminalDevice stdout
    let o = o' {color = color o' && isTerminal}
    f <- maybe getContents readFile (file o)
    doc <- pretty o f
    if inPlace o
        then case file o of
                 Just f -> withFile f WriteMode (`displayIO` doc)
                 Nothing ->
                     die "stylish-cabal: --in-place specified, but I'm reading from stdin"
        else displayIO stdout doc
