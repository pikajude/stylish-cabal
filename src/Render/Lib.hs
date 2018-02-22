{-# Language ImplicitParams #-}
{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}

module Render.Lib
    ( P(..)
    , renderBlockHead
    , showVersionRange
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
import Render.Options
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
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

renderTestedWith ts = do
    tests <- mapM (\(compiler, vers) -> showVersioned (showCompiler compiler, vers)) ts
    return $ fillSep . punctuate comma $ tests
  where
    showCompiler (OtherCompiler x) = x
    showCompiler HaskellSuite {} =
        error "Not sure what to do with HaskellSuite value in tested-with field"
    showCompiler x = show x

showVersioned :: (String, VersionRange) -> Render Doc
showVersioned (pn, v')
    | v' == anyVersion = pure $ string pn
    | otherwise = fmap (string pn <+>) (showVersionRange v')

showVersionRange r = do
    opts <- ask
    return $
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
            parens .
        (if simplifyVersions opts
             then simplifyVersionRange
             else id) $
        r

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

renderBlockHead (If c) = (dullblue "if" <+>) <$> showPredicate c
renderBlockHead x = pure $ r x
  where
    r CustomSetup = dullgreen "custom-setup"
    r (SourceRepo_ k) = dullgreen "source-repository" <+> showKind k
      where
        showKind RepoHead = "head"
        showKind RepoThis = "this"
        showKind (RepoKindUnknown y) = string y
    r (Library_ Nothing) = dullgreen "library"
    r (Library_ (Just l)) = dullgreen "library" <+> string l
    r (ForeignLib_ l) = dullgreen "foreign-library" <+> string l
    r (Exe_ e) = dullgreen "executable" <+> string e
    r (TestSuite_ t) = dullgreen "test-suite" <+> string t
    r (Benchmark_ b) = dullgreen "benchmark" <+> string b
    r (Flag_ s) = dullgreen "flag" <+> string s
    r Else = dullblue "else"
    r _ = error "unreachable"

showPredicate :: Condition ConfVar -> Render Doc
showPredicate (Var x) = showVar x
showPredicate (CNot p) = fmap (dullmagenta (string "!") <>) (maybeParens p)
showPredicate (CAnd a b) =
    liftM2 (\x y -> x <+> dullblue (string "&&") <+> y) (maybeParens a) (maybeParens b)
showPredicate (COr a b) =
    liftM2 (\x y -> x <+> dullblue (string "||") <+> y) (maybeParens a) (maybeParens b)
showPredicate (Lit b) = pure $ string $ show b

maybeParens p =
    case p of
        Lit {} -> showPredicate p
        Var {} -> showPredicate p
        CNot {} -> showPredicate p
        _ -> parens <$> showPredicate p

showVar :: ConfVar -> Render Doc
showVar (Impl compiler vers) = do
    v <- showVersioned (map toLower $ show compiler, vers)
    pure $ dullgreen $ string "impl" <> parens (dullblue v)
showVar (Flag f) =
    pure $ dullgreen $ string "flag" <> parens (dullblue $ string (unFlagName f))
showVar (OS w) =
    pure $ dullgreen $ string "os" <> parens (dullblue $ string $ map toLower $ show w)
showVar (Arch a) =
    pure $ dullgreen $ string "arch" <> parens (dullblue $ string $ map toLower $ show a)
