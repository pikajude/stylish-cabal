{-# Language RecordWildCards #-}

module StylishCabal where

import Data.Monoid
import Text.PrettyPrint.ANSI.Leijen (line)

import Parse
import Render
import Transform

pretty i str = do
    m <- parse str
    return $ uncurry (blockBodyToDoc i) (toBlocks m) <> line
