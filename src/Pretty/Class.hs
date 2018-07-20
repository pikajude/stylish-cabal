{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pretty.Class where

import Comment
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
import Distribution.Types.TestType
import Distribution.Types.Version
import Language.Haskell.Extension (Extension, Language)
import Parse
import Prelude hiding ((<$>))
import Text.Parsec.Error
import Text.PrettyPrint.ANSI.Leijen hiding (Pretty(..))

class Stylish a where
    parseValue :: proxy a -> ParsecParser a
    default parseValue :: Parsec a =>
        proxy a -> ParsecParser a
    parseValue _ = parsec
    stylishField :: Int -> Bool -> a -> Doc
    default stylishField :: Pretty a =>
        Int -> Bool -> a -> Doc
    stylishField n _ x = alignTo n $ string (show $ pretty x)

instance Stylish SpecVersion

instance Stylish PackageName

instance Stylish Version

instance Stylish FreeText

instance Stylish TestType

instance Stylish SpecLicense

instance Stylish BuildType

instance Stylish Bool

instance Stylish Token

instance Stylish Language

instance Stylish Token'

instance Stylish FilePathNT

instance (Parsec a, Stylish a) => Stylish (MQuoted a) where
    stylishField n b = stylishField n b . unpack

instance (Parsec a, Stylish a) => Stylish (Identity a) where
    stylishField n b = stylishField n b . unpack

instance Stylish Dependency where
    stylishField n b d =
        if b
            then go
            else alignTo (n - 2) (comma <+> go)
      where
        go = string (show $ pretty d)

instance Stylish ModuleName

instance Stylish TestedWith

instance (Pretty b, Parsec b, Newtype b x, Stylish b) => Stylish (List FSep b x) where
    stylishField = stylishList fillSep

instance (Pretty b, Parsec b, Newtype b x, Stylish b) =>
         Stylish (List NoCommaFSep b x) where
    stylishField = stylishList fillSep

instance (Pretty b, Parsec b, Newtype b x, Stylish b) => Stylish (List VCat b x) where
    stylishField = stylishList vsep

instance (Pretty b, Parsec b, Newtype b x, Stylish b) =>
         Stylish (List CommaVCat b x) where
    stylishField = stylishList vsep

stylishList ::
       forall b x sep. (Newtype b x, Stylish b)
    => ([Doc] -> Doc)
    -> Int
    -> Bool
    -> List sep b x
    -> Doc
stylishList x n b'' =
    x .
    map (\(b', elm) -> stylishField n b' $ (pack :: x -> b) elm) .
    zip ((b'' && True) : repeat False) . unpack

alignTo n d = column $ \k -> indent (n - k) d

fieldPrinter ::
       forall b a ann. Stylish b
    => Proxy b
    -> Int
    -> (Name ann, [FieldLine ann])
    -> ReaderT CabalSpecVersion (Except ParseError) Doc
fieldPrinter _ width (Name _ n, fvs) = do
    spec <- ask
    ls <- liftEither $ runParsec (parseValue (Proxy :: Proxy b)) spec fvs
    return $
        case ls of
            Nothing -> dullblue (bs n) <> colon
            Just ls' ->
                leftJustify (dullblue (bs n) <> colon) (width + 1) ' ' <+>
                column
                    (\k' -> cat (zipWith (show' k') (True : repeat False) $ N.toList ls'))
  where
    show' n b (NotComment x) = cat $ map (stylishField n b) (N.toList x)
    show' n False (MkComment b) = alignTo n $ yellow $ bs b
    show' _ _ (MkComment b) = yellow $ bs b

leftJustify d len chr = width d $ \n -> string (replicate (len - n) chr)

bs :: B.ByteString -> Doc
bs m = string (toString m)

trail y [] = y
trail y xs = y <$> vcat xs
