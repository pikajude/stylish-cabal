{-# Language ImplicitParams #-}
{-# Language CPP #-}

-- | Cabal file formatter.
module StylishCabal
      -- * Formatting Cabal files
    ( pretty
    , prettyOpts
    , RenderOptions(..)
    , render
      -- * Parsing utilities
    , parsePackageDescription
    , readPackageDescription
    , Result(..)
    , result
    , printWarnings
    , displayError
    -- * Reexports
    , Default(..)
    , Doc
    , plain
    , displayIO
    , displayS
    ) where

import Data.Default
import Distribution.PackageDescription (GenericPackageDescription)
import Text.PrettyPrint.ANSI.Leijen hiding ((<>), pretty)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif
import Parse
import Render
import Render.Options
import Transform

-- | @pretty pkg@ produces a colorized, formatted textual representation of
-- a given 'Distribution.PackageDescription.GenericPackageDescription',
-- using 'Default' options.
--
-- To remove syntax highlighting, you can use 'plain'.
pretty :: GenericPackageDescription -> Doc
pretty = prettyOpts def

-- | 'pretty' with specified options.
prettyOpts :: RenderOptions -> GenericPackageDescription -> Doc
prettyOpts opts gpd = runReader (uncurry blockBodyToDoc $ toBlocks gpd) opts <> line

-- | Render the given 'Doc' with the given width.
render :: Int -> Doc -> SimpleDoc
render = renderSmart 1.0
