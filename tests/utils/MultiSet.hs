{-# Language NoMonomorphismRestriction #-}

module MultiSet where

import qualified Data.Map                      as M
import           Prelude.Compat

newtype MultiSet a = MultiSet { unMultiSet :: M.Map a Int } deriving (Eq, Ord, Show)

fromList = MultiSet . foldr (\v -> M.insertWith (+) v 1) M.empty
