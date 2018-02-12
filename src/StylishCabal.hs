{-# Language NoMonomorphismRestriction #-}
{-# Language OverloadedStrings #-}

module StylishCabal where

import Data.Either
import Data.List
import Debug.Trace
import System.IO
import Text.PrettyPrint.ANSI.Leijen

import Types.Block
import Types.Field
import Render
import Parse

exampleFile :: File
exampleFile =
    [ Field $
      BuildDepends
          [("base", "== 4.*"), ("attoparsec", ">= 0.12"), ("text", ">= 4.3 && < 4.5")]
    , Field $ TextField "name" "attoparsec"
    , Field $
      TextField
          "exposed-modules"
          (sep [ "Module1"
               , "Module2"
               , "Module1"
               , "Module2"
               , "Module1"
               , "Module2"
               , "Module1"
               , "Module2"
               ])
    , Block
          (string "library")
          [ Field $ TextField "main-is" "Main.hs"
          , Block (string "if impl(ghc)") [Field $ TextField "ghc-options" "-O2"]
          ]
    ]
