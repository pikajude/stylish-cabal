{-# Language FlexibleContexts #-}

module Render
    ( blockBodyToDoc
    ) where

import Data.List hiding (group)
import Data.List.Split
import Data.Maybe
import Data.Ord
import Distribution.Types.Dependency
import Distribution.Types.PackageName
import Distribution.Version
import Text.PrettyPrint.ANSI.Leijen

import Render.Lib
import Types.Block
import Types.Field

fieldValueToDoc _ k (Field _ (Dependencies ds)) =
    buildDepsToDoc k $ map (\(Dependency pn v) -> (P $ unPackageName pn, v)) ds
fieldValueToDoc _ k (Field _ n) = colon <> indent (k + 1) (align $ val' n)
  where
    val' (Str x) = string x
    val' (File x) = filepath x
    val' (Version v) = string $ showVersion v
    val' (CabalVersion v)
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
    val' (Extensions es) = val' (LongList $ map showExtension es)
    val' Dependencies {} = error "nonsense"
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
