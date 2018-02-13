{-# Language RecordWildCards #-}

module StylishCabal where

import System.IO
import Text.PrettyPrint.ANSI.Leijen

import Parse
import Render
import Transform

data Opts = Opts
    { file :: Maybe FilePath
    , inPlace :: Bool
    , color :: Bool
    , width :: Int
    , indent :: Int
    } deriving (Show)

pretty Opts{..} str = do
    m <- parse str
    let doc = uncurry (blockBodyToDoc indent) (toBlocks m) <> line
    return $ renderPretty 1.0 width $ if color then doc else plain doc
