{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Parse where

import Comment
import Control.Arrow
import Control.Monad.Except
import qualified Data.ByteString as B
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Maybe
import Data.Semigroup ((<>))
import Debug.Trace
import Distribution.CabalSpecVersion
import qualified Distribution.Compat.CharParsing as P
import Distribution.PackageDescription.Parsec
import Distribution.Parsec.Class
import Distribution.Parsec.Common
import Distribution.Parsec.FieldLineStream
import Distribution.Parsec.Parser
import Distribution.Types.GenericPackageDescription
import Distribution.Types.PackageDescription
import Distribution.Utils.Generic
import Distribution.Version
import Lens.Micro
import Stylish.Monad
import Text.Parsec (runParser)
import Text.Parsec.Error

parseCabalFile str = do
    case runParseResult $ parseGenericPackageDescription str of
        (_, Right (GenericPackageDescription {packageDescription})) -> do
            x <-
                liftEither $
                left (ParseError . (,) Nothing) $
                readFields $
                B.map
                    (\x ->
                         if x == 127
                             then 32
                             else x)
                    str
            let cabalVer = specVersion packageDescription
                specVer
                    | cabalVer >= mkVersion [2, 3] = CabalSpecV2_4
                    | cabalVer >= mkVersion [2, 1] = CabalSpecV2_2
                    | cabalVer >= mkVersion [1, 25] = CabalSpecV2_0
                    | cabalVer >= mkVersion [1, 23] = CabalSpecV1_24
                    | cabalVer >= mkVersion [1, 21] = CabalSpecV1_22
                    | otherwise = CabalSpecOld
            pure (specVer, x)
        y -> throwError $ MiscError $ show y

-- list of FieldLines -> list of (Maybe "associated comment", "field content")
groupLines ::
       Show ann => Bool -> [FieldLine ann] -> [(Maybe [B.ByteString], FieldLineStream)]
groupLines _ [] = []
groupLines stripCommas fc =
    case spanMaybe isComment fc of
        (comms, fs) ->
            let (plainfields, rest) = spanMaybe isLine fs
             in ( guard (not $ null comms) >> Just comms
                , toLineStream $
                  over _head stripLeadingComma $
                  over _last stripTrailingComma $ plainfields) :
                groupLines stripCommas rest
  where
    stripLeadingComma
        | stripCommas = unComma
        | otherwise = B.dropWhile (== 32)
    stripTrailingComma
        | stripCommas = B.reverse . unComma . B.reverse
        | otherwise = B.reverse . B.dropWhile (== 32) . B.reverse
    toLineStream [x] = FLSLast x
    toLineStream (x:xs) = FLSCons x (toLineStream xs)
    toLineStream [] = FLSLast ""
    isComment (FieldComment _ b) = Just $ stripComment b
    isComment _ = Nothing
    isLine (FieldLine _ b) = Just b
    isLine _ = Nothing

runParsec ::
       Show ann
    => Bool
    -> ParsecParser a
    -> CabalSpecVersion
    -> [FieldLine ann]
    -> Either (FieldLineStream, ParseError) [Commented a]
runParsec stripComma p spec fls = sequence $ map parse pairs
    -- fmap (nonEmpty . catNots) $ sequence $ zipWith (maybeParse p) [0 ..] chunks
  where
    pairs = groupLines stripComma fls
    parse (xs, y) =
        case runParser (unPP p spec <* P.eof) [] "<input file>" y of
            Left err -> Left (y, err)
            Right y' -> Right $ MkCommented xs y'

stripComment = B.dropWhile (== 32)

-- strip a leading or trailing comma from a CommaVSep list
-- we parse each section of the list separately because we have no other option, but
-- leading or trailing commas will screw up the parser
unComma = strip
  where
    strip = B.dropWhile (== 32) . B.dropWhile (== 44) . B.dropWhile (== 32)
