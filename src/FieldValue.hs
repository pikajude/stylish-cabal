{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}

module FieldValue where

import Data.Functor.Identity
import Data.List (intersperse)
import Data.Maybe
import Data.Proxy
import Distribution.CabalSpecVersion
import Distribution.Compat.Newtype
import Distribution.ModuleName
import Distribution.Parsec.Class
import Distribution.Parsec.Field
import Distribution.Parsec.FieldLineStream
import Distribution.Parsec.Newtypes
import Distribution.Pretty
import Distribution.Types.BuildType (BuildType)
import Distribution.Types.Dependency
import Distribution.Types.PackageName
import Distribution.Types.SourceRepo
import Distribution.Types.TestType (TestType)
import Distribution.Version
import GHC.TypeLits
import Language.Haskell.Extension
import PrintContext
import Text.Parsec (runParser)
import qualified Text.Parsec as P
import Text.PrettyPrint.ANSI.Leijen hiding (Pretty(..))
import Text.PrettyPrint.ANSI.Leijen.Internal (fold)

class Parsec n =>
      FieldValue n
    where
    pprField :: proxy n -> Maybe Int -> n -> Doc
    default pprField :: Pretty n =>
        proxy n -> Maybe Int -> n -> Doc
    pprField _ n b = string $ show $ pretty b

alignTo Nothing d = d
alignTo (Just n) d = column $ \k -> string (replicate (n - k) ' ') <> d

instance FieldValue Bool

instance FieldValue FreeText

instance FieldValue Token'

instance FieldValue SpecVersion

instance FieldValue Dependency

instance FieldValue SpecLicense

instance FieldValue PackageName

instance FieldValue VersionRange

instance FieldValue Language

instance FieldValue FilePathNT

instance FieldValue Extension

instance FieldValue BuildType

instance FieldValue TestType

instance FieldValue RepoType

instance FieldValue ModuleName

instance FieldValue a => FieldValue (MQuoted a) where
    pprField _ n mq = pprField (Proxy :: Proxy a) n (unpack mq)

instance FieldValue a => FieldValue (Identity a) where
    pprField _ n mq = pprField (Proxy :: Proxy a) n (unpack mq)

instance (FieldValue nt, Newtype nt uw) => FieldValue (List VCat nt uw) where
    pprField _ = pprListWith (align . hcat . intersperse hardline)

instance FieldValue (List'' CommaVCat Dependency) where
    pprField _ n depList = pprListWithPrint pprDependency (commaVCat n) n depList
      where
        deps = repackList depList
        longestDepName =
            maximum $ map (length . unPackageName . depPkgName . runIdentity) deps
        pprDependency n (Identity dep) =
            fill (longestDepName + 1) (pprField Proxy n (depPkgName dep)) <+>
            cataVersionRange showF (depVerRange dep)

showF AnyVersionF = empty
showF (OrLaterVersionF mv) = ">=" <+> string (show $ pretty mv)
showF (WildcardVersionF mv) = "==" <+> string (show $ pretty mv) <> ".*"

instance (FieldValue nt, Newtype nt uw) => FieldValue (List FSep nt uw) where
    pprField _ = pprListWith (align . fillSep)

instance (FieldValue nt, Newtype nt uw) => FieldValue (List NoCommaFSep nt uw) where
    pprField _ = pprListWith (align . fillSep)

type List' sep f r = List sep (f r) r

type List'' sep r = List' sep Identity r

commaVCat n (d:ds) =
    hcat $
    intersperse hardline $
    alignTo n d : map (\d' -> alignTo (subtract 2 `fmap` n) (comma <+> d')) ds

repackList :: Newtype nt uw => List sep nt uw -> [nt]
repackList = map pack . unpack

joinWith sep a b = a <> sep <> b

pprListWith = pprListWithPrint (pprField Proxy)

pprListWithPrint pp cat n fs' = cat $ map (pp n) $ repackList fs'

pprType lines pctx = f "type" lines pctx
  where
    f =
        case _sections pctx of
            ("source-repository":_) -> fieldPrinter (Proxy @RepoType)
            ("test-suite":_) -> fieldPrinter (Proxy @TestType)

fieldPrinter ::
       forall a proxy ann. (FieldValue a, Show ann, Show a)
    => proxy a
    -> String
    -> [FieldLine ann]
    -> PrintContext
    -> Doc
fieldPrinter p fName lines PrintContext {_valueColumn = width} =
    dullblue (string fName <> colon) <+> fieldValue
  where
    fieldValue =
        case runParser
                 (unPP (parsec @a) CabalSpecV1_24 <* P.eof)
                 []
                 ("field: " ++ show fName)
                 (fieldLinesToStream lines) of
            Right val -> alignTo (Just width) $ pprField p (Just width) val
            Left e -> error $ show e
