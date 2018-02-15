module StylishCabal
    ( pretty
    , displayError
    , printWarnings
    , Result (..)
    , parse
    ) where

import Data.Monoid
import Text.PrettyPrint.ANSI.Leijen (line)

import Parse
import Render
import Transform

pretty i gpd = uncurry (blockBodyToDoc i) (toBlocks gpd) <> line
