{-# OPTIONS_GHC -freduction-depth=0 #-}

module Instances.TreeDiff.Language where

import Data.TreeDiff
import Language.Haskell.Extension (Extension, KnownExtension, Language)

-- This are big enums, so they are in separate file.
--
instance ToExpr Extension

instance ToExpr KnownExtension

instance ToExpr Language
