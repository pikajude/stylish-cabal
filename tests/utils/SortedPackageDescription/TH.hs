{-# Language NoMonomorphismRestriction #-}
{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language TypeSynonymInstances #-}
{-# Language DefaultSignatures #-}
{-# Language TypeFamilies #-}

module SortedPackageDescription.TH where

import Control.Monad
import Data.Char (toUpper)
import Data.List (sort)
import MultiSet
import Debug.Trace
import GHC.Generics
import Language.Haskell.TH
import Prelude.Compat

class Sortable a where
    type MkSortable a :: *
    sortable :: a -> MkSortable a

instance (Sortable a, Sortable b) => Sortable (Either a b) where
    type MkSortable (Either a b) = Either (MkSortable a) (MkSortable b)
    sortable (Left a) = Left (sortable a)
    sortable (Right a) = Right (sortable a)

instance (Sortable a, Sortable b) => Sortable (a, b) where
    type MkSortable (a, b) = (MkSortable a, MkSortable b)
    sortable (a, b) = (sortable a, sortable b)

instance Sortable a => Sortable (Maybe a) where
    type MkSortable (Maybe a) = Maybe (MkSortable a)
    sortable (Just x) = Just (sortable x)
    sortable Nothing = Nothing

instance (Ord (MkSortable a), Sortable a) => Sortable [a] where
    type MkSortable [a] = MultiSet (MkSortable a)
    sortable xs = fromList $ map sortable xs

appsT [] = error "appsT []"
appsT [x] = x
appsT (x:y:zs) = appsT ((appT x y) : zs)

prim ns = fmap concat $ forM ns $ \ n ->
    sequence
        [ instanceD
              (cxt [])
              [t|Sortable $(conT n)|]
              [ tySynInstD ''MkSortable (tySynEqn [conT n] (conT n))
              , funD 'sortable [clause [] (normalB [|id|]) []]
              ]
        ]

deriveSortable = deriveSortable_ ""

deriveSortable_ prefix ns = fmap concat $ forM ns $ \ n -> do
    TyConI x <- reify n
    (dty, sortableD) <- mkSortableDataD prefix x
    let tyhead = conT n
        tyvarhead = conT dty
    sequence $
        [ pure sortableD
        , instanceD
              (cxt [])
              [t|Sortable $(tyhead)|]
              [ tySynInstD ''MkSortable (tySynEqn [tyhead] (conT dty))
              , funD 'sortable (mkSortableImpl prefix x)
              ]
        ]

mkSortableDataD prefix (DataD cx tyName [] k cons _) =
    (,) newname <$>
    dataD
        (pure cx)
        newname
        []
        k
        (map (mkSortableCon prefix) cons)
        [derivClause Nothing [[t|Show|], [t|Ord|], [t|Eq|]]]
  where
    newname = sortedTyName prefix tyName
mkSortableDataD prefix (NewtypeD cx tyName [] k con _) =
    (,) newname <$>
    newtypeD
        (pure cx)
        newname
        []
        k
        (mkSortableCon prefix con)
        [derivClause Nothing [[t|Show|], [t|Ord|], [t|Eq|]]]
  where
    newname = sortedTyName prefix tyName

mkSortableCon prefix (RecC recName fields) =
    recC (sortedTyName prefix recName) (map mkSortedField fields)
  where
    mkSortedField (varname, bng, varty) =
        varBangType
            (sortedValName varname)
            (bangType (pure bng) [t|MkSortable $(pure varty)|])
mkSortableCon prefix (NormalC nm tys) =
    normalC (sortedTyName prefix nm) (map mkSortedField tys)
  where
    mkSortedField (bng, varty) = bangType (pure bng) [t|MkSortable $(pure varty)|]
mkSortableCon _ x = error $ "Unhandled case in mkSortableCon: " ++ show x

sortedTyName pref = mkName . ("MkSort" ++) . (pref ++) . nameBase

sortedValName = mkName . ("mkSort" ++) . firstToUpper . nameBase
  where
    firstToUpper (x:xs) = toUpper x : xs
    firstToUpper [] = []

mkSortableImpl pref (DataD _ _ _ _ cons _) = map (mkSortableImplClause pref) cons
mkSortableImpl pref (NewtypeD _ _ _ _ con _) = [mkSortableImplClause pref con]
mkSortableImpl pref x = error $ show x

mkSortableImplClause pref con = do
    let (n, vars) = extract con
    vs <- replicateM vars (newName "arg")
    clause
        [conP n (map varP vs)]
        (normalB (appsE (conE (sortedTyName pref n) : map (dosort . varE) vs)))
        []
  where
    dosort v = [|sortable $(v)|]
    extract (RecC n vs) = (n, length vs)
    extract (NormalC n vs) = (n, length vs)
    extract x = error $ "Unhandled case in extract: " ++ show x
