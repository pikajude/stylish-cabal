module Render where

import Data.List
import Text.PrettyPrint.ANSI.Leijen
import Types.Block
import Types.Field

fieldValueToDoc k (TextField _ n) = colon <> indent (k + 1) (align n)
fieldValueToDoc k (BuildDepends bs)
    | k == 0 = deps
    | otherwise = colon <> indent (k - 1) deps
  where
    lsep
        | k == 0 = string ": "
        | otherwise = string "  "
    deps = encloseSep lsep empty (string ", ") (map showField bs)
    longest = maximum $ map (length . fst) bs
    showField (fieldName, fieldVal) =
        width (string fieldName) $ \fn -> indent (longest - fn + 1) (string fieldVal)

fieldsToDoc fields =
    vcat $
    map (\field ->
             width (string (fieldName field)) $ \fn ->
                 (fieldValueToDoc (longestField - fn) field))
        fields
  where
    longestField = maximum $ map (length . fieldName) fields

blocksToDoc blks =
    fieldsToDoc fields <$$>
    vcat ((empty :) $ intersperse empty $ map renderBlock sections)
  where
    (fields, sections) = splitBlocks blks
    longestFieldName = maximum $ map (length . fieldName) fields
    renderBlock (d, bs) = d <$$> indent 2 (align $ blocksToDoc bs)
