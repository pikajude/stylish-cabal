-- | Cabal file formatter.
module StylishCabal
  ( -- * Formatting Cabal files
    pretty
  , prettyOpts
  , RenderOptions(..)
  , render
  -- * Parsing utilities
  , parsePackageDescription
  , readPackageDescription
  , Result(..)
  , PError(..)
  , PWarning(..)
  , result
  , printWarnings
  , displayError
  -- * Reexports
  , Default(..)
  , GenericPackageDescription
  , Doc
  , plain
  , displayIO
  , displayS
  ) where

import Data.Default
import Data.Monoid.Compat
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.Parsec.Common
import Prelude.Compat
import Text.PrettyPrint.ANSI.Leijen hiding ((<>), pretty)

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
