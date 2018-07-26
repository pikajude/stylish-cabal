{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module StylishCabal where

import Control.Monad
import qualified Data.ByteString as B
import Data.ByteString.UTF8 (toString)
import Data.List (intersperse)
import Data.Proxy
import Distribution.ModuleName
import Distribution.PackageDescription.Quirks
import Distribution.Parsec.Field
import Distribution.Parsec.Newtypes
import Distribution.Parsec.Parser
import Distribution.Types.BuildType (BuildType)
import Distribution.Types.Dependency
import Distribution.Types.SourceRepo
import Distribution.Utils.Generic
import FieldValue
import GenPrinters
import Language.Haskell.Extension
import Lens.Micro
import Prelude hiding ((<$>))
import PrintContext
import Text.PrettyPrint.ANSI.Leijen hiding (Pretty(..))

printers
    [ ("name", [t|FreeText|])
    , ("version", [t|SpecVersion|])
    , ("cabal-version", [t|SpecVersion|])
    , ("synopsis", [t|FreeText|])
    , ("description", [t|FreeText|])
    , ("category", [t|FreeText|])
    , ("homepage", [t|FreeText|])
    , ("bug-reports", [t|FreeText|])
    , ("location", [t|FreeText|])
    , ("license", [t|SpecLicense|])
    , ("license-file", [t|FilePathNT|])
    , ("build-type", [t|BuildType|])
    , ("extra-source-files", [t|List VCat FilePathNT String|])
    , ("manual", [t|Bool|])
    , ("default", [t|Bool|])
    , ("exposed-modules", [t|List' VCat MQuoted ModuleName|])
    , ("other-modules", [t|List' VCat MQuoted ModuleName|])
    , ("hs-source-dirs", [t|List'' FSep FilePathNT|])
    , ("default-extensions", [t|List' FSep MQuoted Extension|])
    , ("ghc-options", [t|List NoCommaFSep Token' String|])
    , ("build-depends", [t|List'' CommaVCat Dependency|])
    , ("default-language", [t|Language|])
    , ("main-is", [t|FilePathNT|])
    , ("buildable", [t|Bool|])
    ]
    (\e ->
         [|if $(e) == "type"
               then pprType
               else error (show $(e))|])

prettyFile fp = do
    bytes <- B.readFile fp
    case patchQuirks bytes of
        (patched, bytes') -> do
            when patched $ putStrLn "idk"
            case readFields' bytes' of
                Left perr -> error $ show perr
                Right (fs, _, comments) -> do
                    return $ docFields fs emptyPrintContext

docFields fs ctx =
    column $ \c' ->
        vcat . intersperse empty $
        vcat (map (\(n, a) -> docField n a (newCtx c')) topLevelFields) :
        map (docSection (newCtx c')) sections
  where
    newCtx c' = ctx & valueColumn .~ (longestField + c')
    (topLevelFields, sections) = spanMaybe isField fs
    isField (Field n fls) = Just (getName n, fls)
    isField _ = Nothing
    longestField = maximum (map (B.length . fst) topLevelFields) + 2

docSection ctx (Section (Name _ b) sargs fs) =
    dullyellow (bs b) <+> docArgs sargs <$> ("  " <> align (docFields fs newCtx))
  where
    newCtx = ctx & sections %~ (:) b
    docArgs = hcat . map docArg
    docArg (SecArgName _ b) = bs b
    docArg (SecArgOther _ b) = dullmagenta (bs b)
docSection ctx f = docFields [f] ctx

bs n = string (toString n)
