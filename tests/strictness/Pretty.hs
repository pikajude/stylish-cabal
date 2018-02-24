{-# Language OverloadedStrings #-}
{-# Language NoMonomorphismRestriction #-}

module Pretty where

import Data.Bifunctor
import Generics.SOP (Associativity(..))
import Prelude.Compat
import Test.StrictCheck.Observe
import Test.StrictCheck.Shaped
import Text.PrettyPrint.ANSI.Leijen

pprint = showThunk False "_" 10

showThunk a b c d = flip displayS "" $ renderPretty 1.0 90 $ go a b c (renderfold d)
  where
    go _ thunk _ (RWrap T) = thunk
    go qualify thunk prec (RWrap (E pd)) =
        case pd of
            ConstructorD name [] -> withParens False (string $ qualify' name)
            ConstructorD name fields ->
                withParens (prec > 10 && not (null fields)) $
                string (qualify' name) <$$>
                indent 2 (align (sep (map (go qualify thunk 11) fields)))
            RecordD name [] -> withParens (prec > 10) (string (qualify' name))
            RecordD name recfields ->
                withParens (prec > 10) $
                string (qualify' name) <$$>
                indent
                    2
                    (encloseSep (lbrace <> space) (softbreak <> rbrace) (comma <> space) $
                     map
                         (\(fName, x) ->
                              string (qualify' fName) <+>
                              char '=' <+> go qualify thunk 11 x)
                         recfields)
            CustomD fixity list ->
                withParens (prec > fixity) $
                hcat $
                flip fmap list $
                extractEither .
                bimap (string . qualifyEither) (uncurry $ go qualify thunk)
            InfixD name assoc fixity l r ->
                withParens (prec > fixity) $
                let (lprec, rprec) =
                        case assoc of
                            LeftAssociative -> (fixity, fixity + 1)
                            RightAssociative -> (fixity + 1, fixity)
                            NotAssociative -> (fixity + 1, fixity + 1)
                 in fillSep
                        [ go qualify thunk lprec l
                        , string (qualify' name)
                        , go qualify thunk rprec r
                        ]
      where
        withParens False = id
        withParens True = parens
        extractEither = either id id
        qualify' (m, _, n) =
            if qualify
                then m ++ "." ++ n
                else n
        qualifyEither (Left s) = s
        qualifyEither (Right (m, n)) =
            if qualify
                then m ++ "." ++ n
                else n
