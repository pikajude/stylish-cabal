{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}

module Render.Lib
    ( P(..)
    , renderBlockHead
    , renderVersion
    , showExtension
    , moduleDoc
    , rexpModuleDoc
    , showFlibType
    , showFlibOpt
    , filepath
    , renderTestedWith
    , showLicense
    , exeDependencyAsDependency
    ) where

import Data.Char
import Data.List
import Distribution.Compiler
import Distribution.License
import Distribution.ModuleName
import Distribution.PackageDescription
import Distribution.Types.ExeDependency
import Distribution.Types.ForeignLibOption
import Distribution.Types.ForeignLibType
import Distribution.Types.PackageName
import Distribution.Types.UnqualComponentName
import Distribution.Version
import Language.Haskell.Extension
import Text.PrettyPrint.ANSI.Leijen
import Types.Block

newtype P = P
    { unP :: String
    } deriving (Eq)

instance Ord P where
    compare (P "base") (P "base") = EQ
    compare (P "base") _ = LT
    compare _ (P "base") = GT
    compare (P p1) (P p2) = compare p1 p2

showFlibType ForeignLibNativeShared = "native-shared"
showFlibType f = error $ show f

showFlibOpt ForeignLibStandalone = "standalone"

showLicense :: License -> String
showLicense MIT = "MIT"
showLicense BSD2 = "BSD2"
showLicense BSD3 = "BSD3"
showLicense BSD4 = "BSD4"
showLicense PublicDomain = "PublicDomain"
showLicense ISC = "ISC"
showLicense (MPL v) = showL "MPL" (Just v)
showLicense (LGPL v) = showL "LGPL" v
showLicense (GPL v) = showL "GPL" v
showLicense (AGPL v) = showL "AGPL" v
showLicense (Apache v) = showL "Apache" v
showLicense OtherLicense = "OtherLicense"
showLicense x = error $ show x

showL :: String -> Maybe Version -> String
showL s Nothing = s
showL s (Just v) = s ++ "-" ++ showVersion v

renderTestedWith =
    fillSep .
    punctuate comma .
    map (\(compiler, vers) -> showVersioned (showCompiler compiler, vers))
  where
    showCompiler (OtherCompiler x) = x
    showCompiler HaskellSuite {} =
        error "Not sure what to do with HaskellSuite value in tested-with field"
    showCompiler x = show x

showVersioned :: (String, VersionRange) -> Doc
showVersioned (pn, v')
    | v' == anyVersion = string pn
    | otherwise = string pn <+> renderVersion v'

renderVersion =
    foldVersionRange'
        empty
        (\v -> green "==" <+> dullyellow (string (showVersion v)))
        (\v -> green ">" <+> dullyellow (string (showVersion v)))
        (\v -> green "<" <+> dullyellow (string (showVersion v)))
        (\v -> green ">=" <+> dullyellow (string (showVersion v)))
        (\v -> green "<=" <+> dullyellow (string (showVersion v)))
        (\v _ -> green "==" <+> dullyellow (string (showVersion v) <> ".*"))
        (\v _ -> green "^>=" <+> dullyellow (string (showVersion v)))
        (\a b -> a <+> green "||" <+> b)
        (\a b -> a <+> green "&&" <+> b)
        parens

filepath :: String -> Doc
filepath x
    | null x = string "\"\""
    | any isSpace x = string $ show x
    | otherwise = string x

moduleDoc = string . intercalate "." . components

rexpModuleDoc (ModuleReexport pkg origname name) =
    maybe empty (\f -> string (unPackageName f) <> colon) pkg <>
    (if origname == name
         then moduleDoc origname
         else moduleDoc origname <+> "as" <+> moduleDoc name)

showExtension (EnableExtension s) = show s
showExtension (DisableExtension s) = "No" ++ show s
showExtension x = error $ show x

exeDependencyAsDependency (ExeDependency pkg comp vers) =
    (P $ unPackageName pkg ++ ":" ++ unUnqualComponentName comp, vers)

renderBlockHead CustomSetup = dullgreen "custom-setup"
renderBlockHead (SourceRepo_ k) = dullgreen "source-repository" <+> showKind k
  where
    showKind RepoHead = "head"
    showKind RepoThis = "this"
    showKind (RepoKindUnknown x) = string x
renderBlockHead (Library_ Nothing) = dullgreen "library"
renderBlockHead (Library_ (Just l)) = dullgreen "library" <+> string l
renderBlockHead (ForeignLib_ l) = dullgreen "foreign-library" <+> string l
renderBlockHead (Exe_ e) = dullgreen "executable" <+> string e
renderBlockHead (TestSuite_ t) = dullgreen "test-suite" <+> string t
renderBlockHead (Benchmark_ b) = dullgreen "benchmark" <+> string b
renderBlockHead (Flag_ s) = dullgreen "flag" <+> string s
renderBlockHead (If c) = dullblue "if" <+> showPredicate c
renderBlockHead Else = dullblue "else"

showPredicate (Var x) = showVar x
showPredicate (CNot p) =
    dullmagenta (string "!") <>
    case p of
        Lit {} -> showPredicate p
        Var {} -> showPredicate p
        _ -> parens (showPredicate p)
showPredicate (CAnd a b) = showPredicate a <+> dullblue (string "&&") <+> showPredicate b
showPredicate (COr a b) = showPredicate a <+> dullblue (string "||") <+> showPredicate b
showPredicate (Lit b) = string $ show b

showVar (Impl compiler vers) =
    dullgreen $
    string "impl" <> parens (dullblue $ showVersioned (map toLower $ show compiler, vers))
showVar (Flag f) = dullgreen $ string "flag" <> parens (dullblue $ string (unFlagName f))
showVar (OS w) =
    dullgreen $ string "os" <> parens (dullblue $ string $ map toLower $ show w)
showVar (Arch a) =
    dullgreen $ string "arch" <> parens (dullblue $ string $ map toLower $ show a)
