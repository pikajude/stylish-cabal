{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Parse where

import Comment
import Control.Arrow
import Control.Monad.Except
import qualified Data.ByteString as B
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Semigroup ((<>))
import Distribution.CabalSpecVersion
import qualified Distribution.Compat.CharParsing as P
import Distribution.PackageDescription.Parsec
import Distribution.Parsec.Class
import Distribution.Parsec.Common
import Distribution.Parsec.FieldLineStream
import Distribution.Parsec.Parser
import Text.Parsec (runParser)
import Text.Parsec.Error

parseCabalFile str = do
    case runParseResult $ parseGenericPackageDescription str of
        ([], Right _) ->
            let Right x = readFields str
             in pure x
        y -> throwError $ show y

runParsec ::
       ParsecParser a
    -> CabalSpecVersion
    -> [FieldLine ann]
    -> Either ParseError (Maybe (NonEmpty (CommentOr a)))
runParsec p spec fls =
    fmap (nonEmpty . catNots) $ sequence $ zipWith (maybeParse p) [0 ..] chunks
  where
    chunks = catFieldLines fls
    catFieldLines (FieldLine pos bs:FieldLine _ bs':fs) =
        catFieldLines $ FieldLine pos (bs <> "\n" <> bs') : fs
    catFieldLines (f@FieldLine {}:fs) = f : catFieldLines fs
    catFieldLines (f@FieldComment {}:fs) = f : catFieldLines fs
    catFieldLines [] = []
    maybeParse _ n (FieldComment pos b)
        | n == length chunks - 1 = pure (MkComment $ stripComment b)
        | otherwise = pure (MkComment $ stripComment b)
    maybeParse p _ (FieldLine pos l) =
        case runParser (unPP p spec <* P.eof) [] "<input file>" (FLSLast l) of
            Left err -> Left err
            Right y -> Right (NotComment (y :| []))
    catNots (NotComment x:NotComment y:ys) = catNots (NotComment (x <> y) : ys)
    catNots (NotComment x:ys) = NotComment x : catNots ys
    catNots (a:bs) = a : catNots bs
    catNots [] = []

stripComment = B.dropWhile (== 32)
