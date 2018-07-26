{-# LANGUAGE TemplateHaskell #-}

module PrintContext where

import qualified Data.ByteString as B
import Lens.Micro.TH

data PrintContext = PrintContext
    { _sections :: [B.ByteString]
    , _valueColumn :: Int
    }

makeLenses ''PrintContext

emptyPrintContext = PrintContext {_sections = [], _valueColumn = 0}
