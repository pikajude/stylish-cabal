{-# LANGUAGE TypeApplications #-}

module Parse.Fields where

import Distribution.Parsec.Class
import Distribution.Parsec.Field
import Distribution.Parsec.Newtypes
import Distribution.Pretty
import Distribution.Types.PackageName
import Distribution.Version
import Parse
import Text.PrettyPrint.ANSI.Leijen hiding (pretty)

data ParseField a = ParseField
    { parseField :: ParsecParser a
    , showField :: a -> Doc
    }

cabalVersionField = ParseField (parsec @SpecVersion) (string . show . pretty)

nameField = ParseField (parsec @PackageName) (string . show . pretty)
