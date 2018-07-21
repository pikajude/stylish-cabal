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
import Text.Parsec (runParser)
import Text.Parsec.Error

parseCabalFile str = do
    case runParseResult $ parseGenericPackageDescription str of
        ([], Right (GenericPackageDescription {packageDescription})) ->
            let Right x = readFields str
                cabalVer = specVersion packageDescription
                specVer
                    | cabalVer >= mkVersion [2, 3] = CabalSpecV2_4
                    | cabalVer >= mkVersion [2, 1] = CabalSpecV2_2
                    | cabalVer >= mkVersion [1, 25] = CabalSpecV2_0
                    | cabalVer >= mkVersion [1, 23] = CabalSpecV1_24
                    | cabalVer >= mkVersion [1, 21] = CabalSpecV1_22
                    | otherwise = CabalSpecOld
             in pure (specVer, x)
        y -> throwError $ show y

-- list of FieldLines -> list of (Maybe "associated comment", "field content")
groupLines :: Show ann => [FieldLine ann] -> [(Maybe [B.ByteString], B.ByteString)]
groupLines [] = []
groupLines fc =
    case spanMaybe isComment fc of
        (comms, fs) ->
            let (plainfields, rest) = spanMaybe isLine fs
             in (guard (not $ null comms) >> Just comms, B.intercalate "\n" plainfields) :
                groupLines rest
  where
    isComment (FieldComment _ b) = Just $ stripComment b
    isComment _ = Nothing
    isLine (FieldLine _ b) = Just b
    isLine _ = Nothing

buildDepsField =
    [ FieldLine (Position 25 23) "base            == 4.*"
    , FieldLine (Position 26 21) ", Cabal           ^>= 2.3"
    , FieldLine (Position 27 21) ", ansi-wl-pprint"
    , FieldComment
          (Position 28 1)
          "                    -- a comment in a build-depends? nani??"
    , FieldLine (Position 29 21) ", bytestring"
    , FieldLine (Position 30 21) ", mtl"
    , FieldLine (Position 31 21) ", utf8-string"
    , FieldLine (Position 32 21) ", parsec"
    ]

runParsec ::
       Show ann
    => ParsecParser a
    -> CabalSpecVersion
    -> [FieldLine ann]
    -> Either ParseError [Commented a]
runParsec p spec fls = sequence $ map parse pairs
    -- fmap (nonEmpty . catNots) $ sequence $ zipWith (maybeParse p) [0 ..] chunks
  where
    pairs = groupLines fls
    parse (xs, y) =
        case runParser (unPP p spec <* P.eof) [] "<input file>" (FLSLast y) of
            Left err -> Left err
            Right y' -> Right $ MkCommented xs y'

stripComment = B.dropWhile (== 32)
