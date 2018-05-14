{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Render.Lib.Haddock where

import Control.Monad.Reader
import Data.Char (isSpace)
import Data.List.Compat
import Data.List.Split
import Data.Monoid.Compat
import Documentation.Haddock.Parser (Identifier)
import Documentation.Haddock.Types
import Prelude.Compat
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))

data FlattenBehavior
  = Flatten
  | WordBreak

data TextPosition
  = ParaStart
  | Body
  deriving (Eq)

data DocContext = DocContext
  { flattenBehavior :: FlattenBehavior
  , textPosition :: TextPosition
  , listContext :: Bool
  }

flattenedBody = withReader $ \d -> d {flattenBehavior = Flatten, textPosition = Body}

inPara = withReader $ \d -> d {textPosition = ParaStart}

inBody = withReader $ \d -> d {textPosition = Body}

inList = withReader $ \d -> d {listContext = True}

defaultDocContext = DocContext WordBreak ParaStart False

-- sadly can't use the built-in haddock markup functionality here. we need
-- to change rendering logic entirely inside a code block (i.e. don't
-- fillSep words).
renderDescription =
  vcat . intersperse (green ".") . map ((`runReader` defaultDocContext) . go) . flatten
  where
    flatten (DocAppend d1 d2) = flatten d1 ++ flatten d2
    flatten d = [d]
    -- flatten inline styling which the haddock parser doesn't preserve
    -- across line boundaries
    go :: DocH () Identifier -> Reader DocContext Doc
    go DocEmpty = pure empty
    go (DocEmphasis d) = enclose (green "/") (green "/") <$> flattenedBody (go d)
    go (DocMonospaced d) = enclose (green "@") (green "@") <$> flattenedBody (go d)
    go (DocBold d) = enclose (green "__") (green "__") <$> flattenedBody (go d)
    go (DocHeader (Header l t)) =
      (green (strBody $ replicate l '=') <+>) <$> inBody (go t)
    go (DocAppend a b) = liftM2 (<>) (go a) (inBody $ go b)
    go (DocParagraph d) = inPara $ go d
    go (DocUnorderedList ds) = do
      docs <-
        forM ds $ \item -> do
          doc <- inList $ inPara $ go item
          return $ hang 2 $ string "*" <+> doc
      return $ vcat docs
    go (DocOrderedList ds) = do
      docs <-
        forM (zip [1 ..] ds) $ \(n, item) -> do
          doc <- inList $ inPara $ go item
          return $ hang 3 $ integer n <> "." <+> doc
      return $ vcat docs
    go (DocCodeBlock cb) = do
      DocContext {..} <- ask
      return $
        case cb of
          DocString s
            | all (`notElem` ['{', '}']) s && not listContext ->
              green ">" <+> arrowblock s
            | listContext && notElem '\n' s -> cat [green "@", string s, green "@"]
          y -> vcat [green "@", goplain y <> green "@"]
    go (DocString s) = do
      DocContext {..} <- ask
      return $
        case flattenBehavior of
          Flatten ->
            case textPosition of
              Body -> strBody s
              ParaStart -> strPara s
          WordBreak ->
            fillSep $
            (if textPosition == ParaStart
               then map2 strPara strBody
               else map strBody) $
            splitWhen isSpace s
    go (DocDefList ds) = do
      docs <-
        forM ds $ \(hdr, body) -> do
          rhdr <- inBody $ go hdr
          rbdy <- inBody $ go body
          return $ enclose (green "[") (green "]") rhdr <+> rbdy
      return $ vcat docs
    go (DocExamples es) =
      return $
      vcat $
      map
        (\Example {..} ->
           vcat $ (green ">>>" <+> string exampleExpression) : map string exampleResult)
        es
    go (DocModule x) = return $ enclose (green "\"") (green "\"") (strBody x)
    go (DocIdentifier (c, x, c2)) =
      return $ enclose (green $ char c) (green $ char c2) (string x)
    go (DocMathDisplay x) = return $ enclose (green "\\[") (green "\\]") (strBody x)
    go (DocMathInline x) = return $ enclose (green "\\(") (green "\\)") (strBody x)
    -- go (DocAName x) = return $ enclose (green "#") (green "#") (strBody x)
    go (DocHyperlink (Hyperlink h l)) =
      return $
      enclose (green "<") (green ">") (string $ h ++ maybe "" (" " ++) (unNl <$> l))
    go (DocPic (Picture p t)) =
      return $
      enclose (green "<<") (green ">>") (string $ p ++ maybe "" (" " ++) (unNl <$> t))
    go x = error $ "Unhandled Haddock AST node: " ++ show x
    unNl =
      map
        (\x ->
           if x == '\n'
             then ' '
             else x)
    goplain (DocString s) = strPara $ escapeHtml s
    goplain (DocAppend a b) = goplain a <> goplain b
    goplain (DocIdentifier (a, b, c)) =
      enclose (green (char a)) (green (char c)) (string b)
    goplain (DocEmphasis x) = enclose (green "/") (green "/") (goplain x)
    goplain (DocModule x) = enclose (green "\"") (green "\"") (string x)
    goplain n = error $ "Unhandled in goplain: " ++ show n

map2 f g (x:xs) = f x : map g xs
map2 _ _ [] = []

arrowblock ('\n':ys) = line <> green "> " <> arrowblock ys
arrowblock (x:xs) = char x <> arrowblock xs
arrowblock "" = empty

escapeHtml "" = ""
escapeHtml ('\n':'\n':xs) = '\n' : '.' : '\n' : escapeHtml xs
escapeHtml ('\n':xs)
  | (spcs, '-':chrs) <- span isSpace xs = '\n' : spcs ++ ('\1' : '-' : escapeHtml chrs)
escapeHtml (c:cs) = c : escapeHtml cs

strPara ('>':'>':'>':cs) = text "\\>>>" <> strBody cs
strPara (x:cs)
    -- >, *, and - are mentioned by the haddock docks, but [ is also
    -- a special character b/c it starts a definition list
  | x `elem` ['>', '*', '-', '['] = char '\\' <> char x <> strBody cs
strPara x = strBody x

strBody ('\n':x) = line <> strPara x
strBody ('{':xs) = "&#x7b;" <> strBody xs
strBody ('}':xs) = "&#x7d;" <> strBody xs
strBody (x:xs)
  | x `elem` ['\\', '/', '\'', '`', '"', '@', '<', '#'] =
    char '\\' <> char x <> strBody xs
  | x == '\1' = char '\\' <> strBody xs
  | otherwise = char x <> strBody xs
strBody "" = empty
