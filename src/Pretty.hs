{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Pretty where

import Comment
import Control.Monad.State
import qualified Data.ByteString as B
import Data.ByteString.UTF8 (toString)
import Data.Functor.Identity
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Ord
import Data.Proxy
import Distribution.ModuleName (ModuleName)
import Distribution.Parsec.Class
import Distribution.Parsec.Field
import Distribution.Parsec.Newtypes
import Distribution.Pretty
import Distribution.Types.BuildType (BuildType)
import Distribution.Types.Dependency (Dependency)
import Distribution.Types.PackageName
import Distribution.Types.SourceRepo
import Distribution.Types.TestType
import Distribution.Version
import Language.Haskell.Extension (Extension, Language)
import Parse
import Parse.Fields
import Prelude hiding ((<$>))
import Stylish.Class
import Stylish.Monad
import Text.Parsec.Error
import Text.PrettyPrint.ANSI.Leijen hiding (Pretty, pretty)

pprFields = go
  where
    go fields = do
        df <- docFields fields
        pure df

docFields fs =
    vcat `fmap`
    mapM
        (\f ->
             case f of
                 CommentLine _ b -> pure $ yellow $ bs $ stripComment b
                 Section (Name _ n) args fs -> do
                     setSectionName n
                     fs' <- docFields fs
                     clearSectionName
                     return $
                         dullyellow (bs n) <+> hcat (map docArg args) <$>
                         ("  " <> align fs') <> hardline
                 Field n fls -> showField n (n, fls))
        fs
  where
    isComment CommentLine {} = True
    isComment _ = False
    showField (Name _ n) =
        case n of
            "cabal-version" -> fieldPrinter (Proxy @SpecVersion) longest
            "name" -> fieldPrinter (Proxy @PackageName) longest
            "version" -> fieldPrinter (Proxy @Version) longest
            "synopsis" -> fieldPrinter (Proxy @FreeText) longest
            "description" -> fieldPrinter (Proxy @FreeText) longest
            "license" -> fieldPrinter (Proxy @SpecLicense) longest
            "license-file" -> fieldPrinter (Proxy @(List FSep FilePathNT _)) longest
            "copyright" -> fieldPrinter (Proxy @FreeText) longest
            "homepage" -> fieldPrinter (Proxy @FreeText) longest
            "bug-reports" -> fieldPrinter (Proxy @FreeText) longest
            "author" -> fieldPrinter (Proxy @FreeText) longest
            "maintainer" -> fieldPrinter (Proxy @FreeText) longest
            "location" -> fieldPrinter (Proxy @FreeText) longest
            "tested-with" -> fieldPrinter (Proxy @(List FSep TestedWith _)) longest
            "category" -> fieldPrinter (Proxy @FreeText) longest
            "build-type" -> fieldPrinter (Proxy @BuildType) longest
            "extra-source-files" -> fieldPrinter (Proxy @(List VCat FilePathNT _)) longest
            "ghc-options" -> fieldPrinter (Proxy @(List NoCommaFSep Token' _)) longest
            "cpp-options" -> fieldPrinter (Proxy @(List NoCommaFSep Token' _)) longest
            "default-language" -> fieldPrinter (Proxy @Language) longest
            "main-is" -> fieldPrinter (Proxy @FilePathNT) longest
            "subdir" -> fieldPrinter (Proxy @FilePathNT) longest
            "buildable" -> fieldPrinter (Proxy @Bool) longest
            "default" -> fieldPrinter (Proxy @Bool) longest
            "manual" -> fieldPrinter (Proxy @Bool) longest
            "extra-libraries" -> fieldPrinter (Proxy @(List VCat Token _)) longest
            "type" ->
                \fls -> do
                    sec <- get
                    case sec of
                        Just "test-suite" -> fieldPrinter (Proxy @TestType) longest fls
                        Just "source-repository" ->
                            fieldPrinter (Proxy @RepoType) longest fls
                        Just x -> error $ "unknown section " ++ show x
            "import" -> fieldPrinter (Proxy @Token') longest
            "exposed-modules" ->
                fieldPrinter (Proxy @(List VCat (MQuoted ModuleName) _)) longest
            "other-modules" ->
                fieldPrinter (Proxy @(List VCat (MQuoted ModuleName) _)) longest
            "default-extensions" ->
                fieldPrinter (Proxy @(List FSep (MQuoted Extension) _)) longest
            "other-extensions" ->
                fieldPrinter (Proxy @(List FSep (MQuoted Extension) _)) longest
            "hs-source-dirs" -> fieldPrinter (Proxy @(List FSep FilePathNT _)) longest
            "build-depends" ->
                fieldPrinter (Proxy @(List CommaVCat (Identity Dependency) _)) longest
            "setup-depends" ->
                fieldPrinter (Proxy @(List CommaVCat (Identity Dependency) _)) longest
            x -> const (pure $ bs x)
    longest = maximum $ map (B.length . unName . fieldName) $ filter (not . isComment) fs
    unName (Name _ f) = f
    maybeIndent _ [] = []
    maybeIndent _ [f@FieldComment {}] = [bs $ fieldToBS f]
    maybeIndent n (f:fs) = go (fieldToBS f) : maybeIndent n fs
      where
        go l
            | "," `B.isPrefixOf` l = string (replicate (n - 2) '‗') <> bs l
            | otherwise = string (replicate n '‗') <> bs l

docArg (SecArgName _ b) = bs b
docArg (SecArgOther _ b) = yellow (bs b)

trail y [] = y
trail y xs = y <$> vcat xs

leftJustify d len chr = width d $ \n -> string (replicate (len - n) chr)

fieldToBS (FieldLine _ l) = l
fieldToBS (FieldComment _ c) = B.dropWhile (== 32) c

pretty' :: Pretty a => a -> Doc
pretty' = string . show . pretty
