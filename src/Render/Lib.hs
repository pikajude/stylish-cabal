{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.Lib
  ( P(..)
  , renderBlockHead
  , showVersionRange
  , moduleDoc
  , rexpModuleDoc
  , filepath
  , renderTestedWith
  , exeDependencyAsDependency
  , renderDescription
  ) where

import Data.Char
import Data.List.Compat
import Data.Monoid.Compat
import Distribution.Compiler
import Distribution.ModuleName
import Distribution.PackageDescription
import Distribution.Pretty
import Distribution.Types.ExeDependency
import Distribution.Types.PackageName
import Distribution.Types.UnqualComponentName
import Distribution.Version
import Prelude.Compat
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))

import Render.Lib.Haddock (renderDescription)
import Render.Options
import Types.Block

newtype P = P
  { unP :: String
  } deriving (Eq)

instance Ord P where
  compare (P "base") (P "base") = EQ
  compare (P "base") _ = LT
  compare _ (P "base") = GT
  compare (P p1) (P p2) = compare p1 p2

renderTestedWith ts =
  fillSep . punctuate comma <$>
  mapM (\(compiler, vers) -> showVersioned (showCompiler compiler, vers)) ts
  where
    showCompiler (OtherCompiler x) = x
    showCompiler (HaskellSuite x) = x
    showCompiler x = show x

showVersioned :: (String, VersionRange) -> Render Doc
showVersioned (pn, v')
  | v' == anyVersion = pure $ string pn
  | otherwise = fmap (string pn <+>) (showVersionRange v')

showVersionRange r = do
  opts <- ask
  return $
    cataVersionRange fold' $
    (if simplifyVersions opts
       then simplifyVersionRange
       else id)
      r
  where
    fold' AnyVersionF = empty
    fold' (ThisVersionF v) = green "==" <+> dullyellow (string (prettyShow v))
    fold' (LaterVersionF v) = green ">" <+> dullyellow (string (prettyShow v))
    fold' (OrLaterVersionF v) = green ">=" <+> dullyellow (string (prettyShow v))
    fold' (EarlierVersionF v) = green "<" <+> dullyellow (string (prettyShow v))
    fold' (OrEarlierVersionF v) = green "<=" <+> dullyellow (string (prettyShow v))
    fold' (WildcardVersionF v) = green "==" <+> dullyellow (string (prettyShow v) <> ".*")
    fold' (MajorBoundVersionF v) = green "^>=" <+> dullyellow (string (prettyShow v))
    fold' (UnionVersionRangesF a b) = a <+> green "||" <+> b
    fold' (IntersectVersionRangesF a b) = a <+> green "&&" <+> b
    fold' (VersionRangeParensF a) = parens a

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
  v <- showVersioned (prettyShow compiler, vers)
  pure $ dullgreen $ string "impl" <> parens (dullblue v)
showVar (Flag f) =
  pure $ dullgreen $ string "flag" <> parens (dullblue $ string (unFlagName f))
showVar (OS w) =
  pure $ dullgreen $ string "os" <> parens (dullblue $ string $ map toLower $ show w)
showVar (Arch a) =
  pure $ dullgreen $ string "arch" <> parens (dullblue $ string $ map toLower $ show a)
