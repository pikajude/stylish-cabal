module Types.Field where

import Text.PrettyPrint.ANSI.Leijen

data Field
    = BuildDepends [(String, String)]
    | Description String
    | TextField String
                Doc
    deriving (Show)

fieldName (TextField s _) = s
fieldName (BuildDepends _) = "build-depends"
