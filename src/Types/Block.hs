module Types.Block where

import Text.PrettyPrint.ANSI.Leijen
import Data.Either
import Types.Field

type File = Blocks

type Blocks = [Block]

data Block
    = Block Doc
            Blocks
    | Field Field
    deriving (Show)

splitBlocks =
    partitionEithers .
    map (\b ->
             case b of
                 Field f -> Left f
                 Block d bs -> Right (d, bs))
