{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stylish.Class where

import Comment
import Control.Arrow (left)
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.ByteString as B
import Data.ByteString.UTF8 (toString)
import Data.Coerce
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as N
import Data.Proxy
import Distribution.CabalSpecVersion
import Distribution.Compat.Newtype
import Distribution.ModuleName
import Distribution.Parsec.Class
import Distribution.Parsec.Field
import Distribution.Parsec.Newtypes
import Distribution.Pretty
import Distribution.Types.BuildType (BuildType)
import Distribution.Types.Dependency
import Distribution.Types.PackageName
import Distribution.Types.SourceRepo
import Distribution.Types.TestType
import Distribution.Types.Version
import Language.Haskell.Extension (Extension, Language)
import Parse
import Prelude hiding ((<$>))
import Stylish.Monad
import Text.Parsec.Error
import Text.PrettyPrint.ANSI.Leijen hiding (Pretty(..))

class Stylish a where
    parseValue :: proxy a -> ParsecParser a
    default parseValue :: Parsec a =>
        proxy a -> ParsecParser a
    parseValue _ = parsec
    stylishField ::
           Int -- ^ starting column for where values should be printed
        -> Bool -- ^ is this the first value in the field?
        -> Commented a
        -> Doc
    default stylishField :: Pretty a =>
        Int -> Bool -> Commented a -> Doc
    stylishField = defaultStylish (string . show . pretty)

defaultStylish f n True (MkCommented (Just comms) x) =
    hcat
        [ hardline
        , alignTo n (vcat $ map (yellow . bs) comms)
        , hardline
        , defaultStylish f n False (MkCommented Nothing x)
        ]
defaultStylish f n _ (MkCommented (Just comms) x) =
    vcat [alignTo n $ vcat $ map (yellow . bs) comms, f x]
defaultStylish f n _ (MkCommented Nothing x) = f x

instance Stylish SpecVersion

instance Stylish PackageName

instance Stylish Version

instance Stylish FreeText where
    stylishField n = defaultStylish (alignTo n . string . show . pretty) n

instance Stylish TestType

instance Stylish SpecLicense

instance Stylish FilePathNT

instance Stylish Language

instance Stylish Extension

instance Stylish BuildType

instance Stylish TestedWith

instance (Parsec a, Stylish a) => Stylish (MQuoted a) where
    stylishField n b = stylishField n b . fmap unpack

instance (Parsec a, Stylish a) => Stylish (Identity a) where
    stylishField n b = stylishField n b . fmap unpack

instance Stylish Token'

instance Stylish Bool

instance Stylish Dependency

instance Stylish RepoType

instance Stylish ModuleName

instance (Pretty b, Parsec b, Newtype b x, Stylish b) => Stylish (List FSep b x) where
    stylishField n = stylishList (alignTo n . fillSep) (\_ _ -> string . show . pretty) n

instance (Pretty b, Parsec b, Newtype b x, Stylish b) =>
         Stylish (List NoCommaFSep b x) where
    stylishField n = stylishList (alignTo n . fillSep) (\_ _ -> string . show . pretty) n

instance (Parsec b, Newtype b x, Pretty b) => Stylish (List VCat b x) where
    stylishField n = stylishList (alignTo n . vsep) (\n _ -> string . show . pretty) n

instance (Parsec b, Newtype b x, Pretty b) => Stylish (List CommaVCat b x) where
    stylishField = stylishList vcat maybeComma
      where
        maybeComma n True c = alignTo n $ string (show $ pretty c)
        maybeComma n False c = alignTo (n - 2) $ "," <+> string (show $ pretty c)

stylishList ::
       forall sep nt uw c. Newtype nt uw
    => ([c] -> Doc)
    -> (Int -> Bool -> nt -> c)
    -> Int
    -> Bool
    -> Commented (List sep nt uw)
    -> Doc
stylishList cat' render' n b =
    defaultStylish
        (cat' .
         zipWith (\b' -> render' n (b && b') . (pack :: uw -> nt)) (True : repeat False) .
         unpack)
        n
        b

alignTo n d = column $ \k -> indent (n - k) d

fieldPrinter ::
       forall b a ann m. (Stylish b, Monad m, Show ann)
    => Proxy b
    -> Int
    -> (Name ann, [FieldLine ann])
    -> StylishT m Doc
fieldPrinter _ width (Name _ n, fvs) = do
    spec <- ask
    ls' <-
        liftEither $ left ParseError $ runParsec (parseValue (Proxy :: Proxy b)) spec fvs
    return $
        leftJustify (dullblue (bs n) <> colon) (width + 1) ' ' <+>
        column (\k' -> cat (zipWith (stylishField k') (True : repeat False) ls'))
  -- where
  --   show' n b (NotComment x) = cat $ map (stylishField n b) (N.toList x)
  --   show' n True (MkComment b) = hardline <> alignTo n (yellow $ bs b)
  --   show' n _ (MkComment b) = alignTo n $ yellow $ bs b

leftJustify d len chr = width d $ \n -> string (replicate (len - n) chr)

bs :: B.ByteString -> Doc
bs m = string (toString m)

trail y [] = y
trail y xs = y <$> vcat xs
