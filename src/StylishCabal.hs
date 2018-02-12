{-# Language NoMonomorphismRestriction #-}
{-# Language OverloadedStrings #-}

module StylishCabal where

import Debug.Trace
import System.IO
import Text.PrettyPrint.ANSI.Leijen

data Block = Field Field
           | Nest [Block]

data Field
    = BuildDepends [(String, String)]
    | Description String
    | TextField String
                Doc

fieldName (TextField s _) = s
fieldName (BuildDepends _) = "build-depends"

fieldValueToDoc k (TextField _ n) = colon <> indent (k + 1) (align n)
fieldValueToDoc k (BuildDepends bs)
    | k == 0 = deps
    | otherwise = colon <> indent (k - 1) deps
  where
    lsep
        | k == 0 = string ": "
        | otherwise = string "  "
    deps = encloseSep lsep empty (string ", ") (map (showField longestField) bs)
    longestField = maximum $ map (length . fst) bs

fieldsToDoc fields =
    vcat $
    map (\field ->
             width (string (fieldName field)) $ \fn ->
                 (fieldValueToDoc (longestField - fn) field))
        fields
  where
    longestField = maximum $ map (length . fieldName) fields

renderBlock (Fields fs) = fieldsToDoc fs
renderBlock (Nest b) = indent 2 (vcat $ map renderBlock b)

showField longest (fieldName, fieldVal) =
    width (string fieldName) $ \fn -> indent (longest - fn + 1) (string fieldVal)

exampleFields = Fields
    [ BuildDepends
          [("base", "== 4.*"), ("attoparsec", ">= 0.12"), ("text", ">= 4.3 && < 4.5")]
    , TextField "name" "attoparsec"
    , TextField
          "exposed-modules"
          (sep
               [ "Module1"
               , "Module2"
               , "Module1"
               , "Module2"
               , "Module1"
               , "Module2"
               , "Module1"
               , "Module2"
               ])
    ]
