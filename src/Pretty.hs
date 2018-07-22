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
import Distribution.Types.BenchmarkType
import Distribution.Types.BuildType (BuildType)
import Distribution.Types.Dependency (Dependency)
import Distribution.Types.ForeignLib
import Distribution.Types.ForeignLibOption
import Distribution.Types.ForeignLibType
import Distribution.Types.LegacyExeDependency
import Distribution.Types.PackageName
import Distribution.Types.PkgconfigDependency
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
                     pushSectionName n
                     fs' <- docFields fs
                     popSectionName
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
            "stability" -> fieldPrinter (Proxy @FreeText) longest
            "license" -> fieldPrinter (Proxy @SpecLicense) longest
            "license-file" -> fieldPrinter (Proxy @(List FSep FilePathNT _)) longest
            "license-files" -> fieldPrinter (Proxy @(List FSep FilePathNT _)) longest
            "copyright" -> fieldPrinter (Proxy @FreeText) longest
            "homepage" -> fieldPrinter (Proxy @FreeText) longest
            "bug-reports" -> fieldPrinter (Proxy @FreeText) longest
            "author" -> fieldPrinter (Proxy @FreeText) longest
            "maintainer" -> fieldPrinter (Proxy @FreeText) longest
            "location" -> fieldPrinter (Proxy @FreeText) longest
            "tested-with" -> fieldPrinter (Proxy @(List FSep TestedWith _)) longest
            "category" -> fieldPrinter (Proxy @FreeText) longest
            "package-url" -> fieldPrinter (Proxy @FreeText) longest
            "repository" -> fieldPrinter (Proxy @FreeText) longest
            "build-type" -> fieldPrinter (Proxy @BuildType) longest
            "lib-version-info" -> fieldPrinter (Proxy @LibVersionInfo) longest
            "extra-source-files" -> fieldPrinter (Proxy @(List VCat FilePathNT _)) longest
            "extra-doc-files" -> fieldPrinter (Proxy @(List VCat FilePathNT _)) longest
            "extra-tmp-files" -> fieldPrinter (Proxy @(List VCat FilePathNT _)) longest
            "include-dirs" -> fieldPrinter (Proxy @(List FSep FilePathNT _)) longest
            "includes" -> fieldPrinter (Proxy @(List FSep FilePathNT _)) longest
            "extra-lib-dirs" -> fieldPrinter (Proxy @(List FSep FilePathNT _)) longest
            "install-includes" -> fieldPrinter (Proxy @(List FSep FilePathNT _)) longest
            "data-files" -> fieldPrinter (Proxy @(List VCat FilePathNT _)) longest
            "c-sources" -> fieldPrinter (Proxy @(List VCat FilePathNT _)) longest
            "ghc-options" -> fieldPrinter (Proxy @(List NoCommaFSep Token' _)) longest
            "hugs-options" -> fieldPrinter (Proxy @(List NoCommaFSep Token' _)) longest
            "nhc98-options" -> fieldPrinter (Proxy @(List NoCommaFSep Token' _)) longest
            "jhc-options" -> fieldPrinter (Proxy @(List NoCommaFSep Token' _)) longest
            "frameworks" -> fieldPrinter (Proxy @(List FSep Token _)) longest
            "ghc-prof-options" ->
                fieldPrinter (Proxy @(List NoCommaFSep Token' _)) longest
            "ghc-shared-options" ->
                fieldPrinter (Proxy @(List NoCommaFSep Token' _)) longest
            "cpp-options" -> fieldPrinter (Proxy @(List NoCommaFSep Token' _)) longest
            "cc-options" -> fieldPrinter (Proxy @(List NoCommaFSep Token' _)) longest
            "ld-options" -> fieldPrinter (Proxy @(List NoCommaFSep Token' _)) longest
            "default-language" -> fieldPrinter (Proxy @Language) longest
            "main-is" -> fieldPrinter (Proxy @FilePathNT) longest
            "data-dir" -> fieldPrinter (Proxy @FilePathNT) longest
            "subdir" -> fieldPrinter (Proxy @FilePathNT) longest
            "buildable" -> fieldPrinter (Proxy @Bool) longest
            "exposed" -> fieldPrinter (Proxy @Bool) longest
            "default" -> fieldPrinter (Proxy @Bool) longest
            "manual" -> fieldPrinter (Proxy @Bool) longest
            "extra-libraries" -> fieldPrinter (Proxy @(List VCat Token _)) longest
            "type" ->
                \fls -> do
                    sec <- get
                    let go s =
                            case s of
                                ("test-suite":_) ->
                                    fieldPrinter (Proxy @TestType) longest fls
                                ("source-repository":_) ->
                                    fieldPrinter (Proxy @RepoType) longest fls
                                ("benchmark":_) ->
                                    fieldPrinter (Proxy @BenchmarkType) longest fls
                                ("foreign-library":_) ->
                                    fieldPrinter (Proxy @ForeignLibType) longest fls
                                -- invalid section nesting, but some hackage packages do it
                                ("library":xs) -> go xs
                                ("executable":xs) -> go xs
                                x -> error $ "unknown section " ++ show x
                    go sec
            "import" -> fieldPrinter (Proxy @Token') longest
            "branch" -> fieldPrinter (Proxy @Token) longest
            "executable" -> fieldPrinter (Proxy @Token) longest
            "tag" -> fieldPrinter (Proxy @Token) longest
            "test-module" -> fieldPrinter (Proxy @ModuleName) longest
            "exposed-modules" ->
                fieldPrinter (Proxy @(List VCat (MQuoted ModuleName) _)) longest
            "other-modules" ->
                fieldPrinter (Proxy @(List VCat (MQuoted ModuleName) _)) longest
            "default-extensions" ->
                fieldPrinter (Proxy @(List FSep (MQuoted Extension) _)) longest
            "extensions" ->
                fieldPrinter (Proxy @(List FSep (MQuoted Extension) _)) longest
            "other-extensions" ->
                fieldPrinter (Proxy @(List FSep (MQuoted Extension) _)) longest
            "hs-source-dirs" -> fieldPrinter (Proxy @(List FSep FilePathNT _)) longest
            "build-depends" ->
                fieldPrinter (Proxy @(List CommaVCat (Identity Dependency) _)) longest
            "build-tools" ->
                fieldPrinter
                    (Proxy @(List CommaFSep (Identity LegacyExeDependency) _))
                    longest
            "setup-depends" ->
                fieldPrinter (Proxy @(List CommaVCat (Identity Dependency) _)) longest
            "options" ->
                fieldPrinter (Proxy @(List FSep (Identity ForeignLibOption) _)) longest
            "pkgconfig-depends" ->
                fieldPrinter
                    (Proxy @(List CommaFSep (Identity PkgconfigDependency) _))
                    longest
            n
                | "x-" `B.isPrefixOf` n -> fieldPrinter (Proxy @FreeText) longest
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

docArg (SecArgName _ b)
    | b == "-any" = " -any"
    | otherwise = bs b
docArg (SecArgStr _ b) = dquotes (bs b)
docArg (SecArgOther _ b)
    | b == "||" = yellow " || "
    | b == "&&" = yellow " && "
    | otherwise = yellow (bs b)

trail y [] = y
trail y xs = y <$> vcat xs

leftJustify d len chr = width d $ \n -> string (replicate (len - n) chr)

fieldToBS (FieldLine _ l) = l
fieldToBS (FieldComment _ c) = B.dropWhile (== 32) c

pretty' :: Pretty a => a -> Doc
pretty' = string . show . pretty
