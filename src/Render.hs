{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-dodgy-imports #-}
{-# Language RecordWildCards #-}
{-# Language StandaloneDeriving #-}
{-# Language FlexibleContexts #-}

module Render
    ( blockBodyToDoc
    ) where

import Data.List.Compat hiding (group)
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
import Documentation.Haddock.Types
import Prelude.Compat hiding ((<$>))
import qualified Prelude.Compat as P
import Text.PrettyPrint.ANSI.Leijen

import Render.Lib
import Render.Options
import Types.Block
import Types.Field

deriving instance Ord ModuleReexport

fieldValueToDoc k (Field _ f) =
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
        RexpModules rms ->
            buildDepsToDoc k $
            map (\rexp -> (P $ show $ rexpModuleDoc rexp, anyVersion)) rms
        n -> val' n <&> \v -> colon <> indent (k + 1) (align v)
  where
    val' (Str x) = pure $ string x
    val' (File x) = pure $ filepath x
    val' (Version v) = pure $ string $ prettyShow v
    val' (CabalVersion (Left v)) = pure $ string $ prettyShow v
    val' (CabalVersion (Right v))
        | v == anyVersion = showVersionRange (orLaterVersion (mkVersion [1, 10]))
        | otherwise = showVersionRange v
    val' (License l) = pure $ string $ prettyShow l
    val' (TestedWith ts) = renderTestedWith ts
    val' (LongList fs) = pure $ vcat $ map filepath fs
    val' (Commas fs) = pure $ fillSep $ punctuate comma $ map filepath fs
    val' (Spaces ls) = pure $ fillSep $ map filepath ls
    val' (Modules ms) = pure $ vcat $ map moduleDoc $ sort ms
    val' (Module m) = pure $ moduleDoc m
    val' (Extensions es) = val' (LongList $ map prettyShow es)
    val' (FlibType ty) = pure $ string $ prettyShow ty
    val' (FlibOptions fs) = val' $ Spaces $ map prettyShow fs
    val' x = error $ show x
fieldValueToDoc k (Description s) = descriptionToDoc k s

descriptionToDoc k paras = do
    n <- asks indentSize
    return $
        (<>) colon $
        nest n $
        case paras of
            DocParagraph {} -> indent (k + 1) ds
            _ -> linebreak <> ds
  where
    ds = renderDescription paras

mixinsToDoc k bs
    | k == 0 = pure $ deps ": "
    | otherwise = pure $ colon <> indent (k - 1) (deps "  ")
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
            (flatAlt (string " (") lparen) -- `mixin-name ( Module` doesn't parse
            (flatAlt (line <> rparen) rparen)
            (string ", ")
    providesDoc (ModuleRenaming ms) = parenthesize $ map renaming ms
    providesDoc (HidingRenaming hs) = string "hiding" <+> parenthesize (map moduleDoc hs)
    providesDoc DefaultRenaming = empty
    renaming (m1, m2) = moduleDoc m1 <+> string "as" <+> moduleDoc m2
    align' n doc = column (\ko -> nesting (\i -> nest (ko - i - n) doc))

buildDepsToDoc :: Int -> [(P, VersionRange)] -> Render Doc
buildDepsToDoc k bs
    | k == 0 = deps ": "
    | otherwise = fmap (\r -> colon <> indent (k - 1) r) (deps "  ")
  where
    deps lsep = do
        fs <- mapM showField $ sortBy (comparing fst) bs
        return $ encloseSep (string lsep) empty (string ", ") fs
    longest = maximum $ map (length . unP . fst) bs
    showField (P fName, fieldVal)
        | fieldVal == anyVersion = pure $ string fName
        | otherwise =
            widthR (string fName) $ \fn -> do
                shown <- showVersionRange fieldVal
                let delt n = indent (n + 1) shown
                 in pure $ flatAlt (delt (longest - fn)) (delt 0)

fieldsToDoc :: [Field] -> Render Doc
fieldsToDoc fs =
    vcat P.<$>
    mapM
        (\field ->
             widthR (dullblue $ string (fieldName field)) $ \fn ->
                 fieldValueToDoc (longestField - fn) field)
        fs
  where
    longestField = maximum $ map (length . fieldName) fs

renderBlock :: Block -> Render Doc
renderBlock (Block t fs blocks) = do
    blkhead <- renderBlockHead t
    body <- indentM . align =<< blockBodyToDoc fs blocks
    return $
        (if isElse t
             then id
             else (<>) line)
            blkhead <$$>
        body

blockBodyToDoc :: [Maybe Field] -> [Block] -> Render Doc
blockBodyToDoc fs blocks = do
    fields <-
        fieldsToDoc
            (if null fs'
                 then buildable'
                 else fs')
    subblocks <- mapM renderBlock blocks
    return $ fields <> vcat (empty : subblocks)
  where
    fs' = catMaybes fs
    buildable' = [fromJust $ stringField "buildable" "True"]
