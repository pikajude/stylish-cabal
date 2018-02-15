{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language RecordWildCards #-}
{-# Language StandaloneDeriving #-}
{-# Language FlexibleContexts #-}

module Render
    ( blockBodyToDoc
    ) where

import Data.List hiding (group)
import Data.List.Split
import Data.Maybe
import Data.Ord
import Distribution.Types.Dependency
import Distribution.Types.IncludeRenaming
import Distribution.Types.LegacyExeDependency
import Distribution.Types.Mixin
import Distribution.Types.ModuleReexport
import Distribution.Types.ModuleRenaming
import Distribution.Types.PackageName
import Distribution.Types.PkgconfigDependency
import Distribution.Types.PkgconfigName
import Distribution.Version
import Prelude hiding ((<$>))
import Text.PrettyPrint.ANSI.Leijen

import Render.Lib
import Types.Block
import Types.Field

deriving instance Ord ModuleReexport

fieldValueToDoc _ k (Field _ f) =
    case f of
        Dependencies ds ->
            buildDepsToDoc k $ map (\(Dependency pn v) -> (P $ unPackageName pn, v)) ds
        ToolDepends ts -> buildDepsToDoc k $ map exeDependencyAsDependency ts
        OldToolDepends ds ->
            buildDepsToDoc k $ map (\(LegacyExeDependency pn v) -> (P pn, v)) ds
        PcDepends ds ->
            buildDepsToDoc k $
            map (\(PkgconfigDependency pn v) -> (P $ unPkgconfigName pn, v)) ds
        Mixins ms -> mixinsToDoc k $ map (\(Mixin pn r) -> (P $ unPackageName pn, r)) ms
        n -> colon <> indent (k + 1) (align $ val' n)
  where
    val' (Str x) = string x
    val' (File x) = filepath x
    val' (Version v) = string $ showVersion v
    val' (CabalVersion v)
        -- section syntax was introduced in Cabal 1.2. if no cabal-version
        -- is specified in the source, we require >=1.2 to be present in
        -- the output
        | v == mkVersion [0] = renderVersion $ orLaterVersion (mkVersion [1, 2])
        -- up until Cabal 1.10, we have to specify '>=' with cabal-version
        | withinRange v (orEarlierVersion (mkVersion [1, 10])) =
            renderVersion $ orLaterVersion v
        | otherwise = string $ showVersion v
    val' (License l) = string $ showLicense l
    val' (TestedWith ts) = renderTestedWith ts
    val' (LongList fs) = vcat $ map filepath fs
    val' (Commas fs) = fillSep $ punctuate comma $ map filepath fs
    val' (Spaces ls) = fillSep $ map filepath ls
    val' (Modules ms) = vcat $ map moduleDoc $ sort ms
    val' (Module m) = moduleDoc m
    val' (RexpModules rs) = vcat $ map rexpModuleDoc $ sort rs
    val' (Extensions es) = val' (LongList $ map showExtension es)
    val' (FlibType ty) = string $ showFlibType ty
    val' (FlibOptions fs) = val' $ Spaces $ map showFlibOpt fs
    val' x = error $ show x
fieldValueToDoc n k (Description s) = descriptionToDoc n k s

descriptionToDoc n k s =
    (<>) colon $
    nest n $
    case paragraphs of
        [p]
            -- i still don't know what this does
         ->
            group $
            flatAlt
                (linebreak <> fillSep (map text $ words p))
                (indent (k + 1) (string p))
        xs -> line <> vcat (intersperse (green dot) (map paragraph xs))
  where
    paragraphs = map (unwords . lines) $ splitOn "\n\n" s
    paragraph t = fillSep (map text $ words t)

mixinsToDoc k bs
    | k == 0 = deps ": "
    | otherwise = colon <> indent (k - 1) (deps "  ")
  where
    deps lsep =
        encloseSep (string lsep) empty (string ", ") $
        map showField $ sortBy (comparing fst) bs
    longest = maximum $ map (length . unP . fst) bs
    hasRequires = any (\(_, c) -> not (isDefaultRenaming $ includeRequiresRn c)) bs
    showField (P fName, i@IncludeRenaming {..})
        | isDefaultIncludeRenaming i = string fName
        | otherwise =
            width (string fName) $ \fn ->
                let delt n =
                        indent
                            (n + 1)
                            (if isDefaultRenaming includeRequiresRn
                                 then providesDoc includeProvidesRn
                                 else group $
                                      align'
                                          9
                                          (providesDoc includeProvidesRn <$>
                                           string "requires" <+>
                                           providesDoc includeRequiresRn))
                    pad doc =
                        if hasRequires
                            then string (replicate (8 - longest) ' ') <> doc
                            else doc
                 in flatAlt (pad $ delt (longest - fn)) (delt 0)
    parenthesize =
        group .
        encloseSep
            (flatAlt (string "( ") lparen)
            (flatAlt (line <> rparen) rparen)
            (string ", ")
    providesDoc (ModuleRenaming ms) = parenthesize $ map renaming ms
    providesDoc (HidingRenaming hs) = string "hiding" <+> parenthesize (map moduleDoc hs)
    providesDoc DefaultRenaming = empty
    renaming (m1, m2) = moduleDoc m1 <+> string "as" <+> moduleDoc m2
    align' n doc = column (\ko -> nesting (\i -> nest (ko - i - n) doc))

buildDepsToDoc k bs
    | k == 0 = deps ": "
    | otherwise = colon <> indent (k - 1) (deps "  ")
  where
    deps lsep =
        encloseSep
            (string lsep)
            empty
            (string ", ")
            (map showField $ sortBy (comparing fst) bs)
    longest = maximum $ map (length . unP . fst) bs
    showField (P fName, fieldVal)
        | fieldVal == anyVersion = string fName
        | otherwise =
            width (string fName) $ \fn ->
                let delt n = indent (n + 1) (renderVersion fieldVal)
                 in flatAlt (delt (longest - fn)) (delt 0)

fieldsToDoc n fs =
    vcat $
    map (\field ->
             width (dullblue $ string (fieldName field)) $ \fn ->
                 fieldValueToDoc n (longestField - fn) field)
        fs
  where
    longestField = maximum $ map (length . fieldName) fs

renderBlock n (Block t fs blocks) =
    (if isElse t
         then id
         else (<>) line)
        (renderBlockHead t) <$$>
    indent n (align $ blockBodyToDoc n fs blocks)

blockBodyToDoc n fs blocks =
    fieldsToDoc
        n
        (if null fs'
             then buildable'
             else fs') <>
    vcat (empty : map (renderBlock n) blocks)
  where
    fs' = catMaybes fs
    buildable' = [fromJust $ stringField "buildable" "True"]
