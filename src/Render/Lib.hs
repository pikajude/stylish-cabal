{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# Language LambdaCase #-}
{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}

module Render.Lib
    ( P(..)
    , renderBlockHead
    , showVersionRange
    , prettyShow
    , moduleDoc
    , rexpModuleDoc
    , filepath
    , renderTestedWith
    , exeDependencyAsDependency
    ) where

import Data.Char
import Data.List.Compat
import Distribution.Compiler
import Distribution.ModuleName
import Distribution.PackageDescription
import Distribution.Text
import Distribution.Types.ExeDependency
import Distribution.Types.PackageName
import Distribution.Types.UnqualComponentName
import Distribution.Version hiding (foldVersionRange)
import Prelude.Compat
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Render.Options
import Types.Block

prettyShow = show . disp

wildcardUpperBound =
    alterVersion $ \lowerBound -> init lowerBound ++ [last lowerBound + 1]

majorUpperBound :: Version -> Version
majorUpperBound =
    alterVersion $ \case
        [] -> [0, 1]
        [m1] -> [m1, 1]
        (m1:m2:_) -> [m1, m2 + 1]

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
        foldVersionRange
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

{------------ EVIL HACK GOES HERE ---------------
-
- Distribution.Version.foldVersionRange' treats both of the following
-   * "(== v) || (> v)"
-   * "(> v) || (== v)"
- as "(>= v)"
-
- Makes sense, right? Nope. ">=" is always Union (This ...) (Later ...),
- whereas if Union (Later ...) (This ...) is present in the source file, we
- must preserve it, otherwise roundtrip tests fail because we're switching
- the order of the Union arguments!
-
- The other solution is to wrap VersionRange in a newtype that disregards
- Union argument ordering for the equality test, but I leave that as an
- exercise to the reader.
-
- Arguments renamed to avoid shadowing
-}
foldVersionRange anyv this later earlier orL orE wildcard major both oneof wrap = fold
  where
    fold AnyVersion = anyv
    fold (ThisVersion v) = this v
    fold (LaterVersion v) = later v
    fold (EarlierVersion v) = earlier v
    fold (UnionVersionRanges (ThisVersion v) (LaterVersion v'))
        | v == v' = orL v
    fold (UnionVersionRanges (ThisVersion v) (EarlierVersion v'))
        | v == v' = orE v
    fold (WildcardVersion v) = wildcard v (wildcardUpperBound v)
    fold (MajorBoundVersion v) = major v (majorUpperBound v)
    fold (UnionVersionRanges v1 v2) = both (fold v1) (fold v2)
    fold (IntersectVersionRanges v1 v2) = oneof (fold v1) (fold v2)
    fold (VersionRangeParens v) = wrap (fold v)

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
