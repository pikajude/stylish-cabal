{-# LANGUAGE DeriveFunctor #-}

module Comment where

import qualified Data.ByteString as B
import Data.List.NonEmpty (NonEmpty)

data Commented a =
    MkCommented (Maybe [B.ByteString])
                a
    deriving (Show, Eq, Functor)
