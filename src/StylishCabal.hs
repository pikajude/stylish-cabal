{-# Language CPP #-}

-- | Cabal file formatter.
module StylishCabal
    ( -- * Formatting Cabal files
      pretty
    , prettyWithIndent
    , render
      -- * Parsing utilities
    , parseCabalFile
    , readCabalFile
    , Result(..)
    , result
    , printWarnings
    , displayError
    -- * Reexports
    , Doc
    , plain
    , displayIO
    , displayS
    ) where

import Text.PrettyPrint.ANSI.Leijen (Doc, displayIO, displayS, line, plain, renderSmart)

import Parse
import Render
import Transform

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif

-- | @pretty pkg@ produces a colorized, formatted textual representation of
-- a given 'Distribution.PackageDescription.GenericPackageDescription',
-- with a default indent width of 2.
--
-- To remove syntax highlighting, you can use 'plain'.
pretty = prettyWithIndent 2

-- | Like 'pretty', but allows you to specify an indent size.
prettyWithIndent i gpd = uncurry (blockBodyToDoc i) (toBlocks gpd) <> line

-- | Render the given 'Doc' with the given width.
render = renderSmart 1.0
