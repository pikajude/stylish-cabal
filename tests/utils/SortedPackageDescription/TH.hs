{-# Language CPP #-}
{-# Language NoMonomorphismRestriction #-}
{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language TypeSynonymInstances #-}
{-# Language DefaultSignatures #-}
{-# Language TypeFamilies #-}

#if !MIN_VERSION_base(4,6,0)
{-# Language ConstraintKinds #-}
#endif

module SortedPackageDescription.TH where

import Control.Monad.Compat
import Data.Char (toUpper)
import Language.Haskell.TH
import MultiSet
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
appsT (x:y:zs) = appsT (appT x y : zs)

prim :: [Name] -> DecsQ
prim ns =
    fmap concat $
    forM ns $ \n ->
        sequence
            [ instanceD
                  (cxt [])
                  [t|Sortable $(conT n)|]
#if MIN_VERSION_template_haskell(2,9,0)
                  [ tySynInstD ''MkSortable (tySynEqn [conT n] (conT n))
#else
                  [ tySynInstD ''MkSortable [conT n] (conT n)
#endif
                  , funD 'sortable [clause [] (normalB [|id|]) []]
                  ]
            ]

#if MIN_VERSION_template_haskell(2,11,0)
#define KIND_ARG _k
#else
#define KIND_ARG
#endif

#if MIN_VERSION_template_haskell(2,12,0)
commonDerivClause = [derivClause Nothing [[t|Show|], [t|Ord|], [t|Eq|]]]
#elif MIN_VERSION_template_haskell(2,11,0)
commonDerivClause = cxt [[t|Show|], [t|Ord|], [t|Eq|]]
#else
commonDerivClause = [''Show, ''Ord, ''Eq]
#endif

deriveSortable :: [Name] -> DecsQ
deriveSortable = deriveSortable_ ""

deriveSortable_ :: String -> [Name] -> DecsQ
deriveSortable_ prefix ns =
    fmap concat $
    forM ns $ \n -> do
        TyConI x <- reify n
        (dty, sortableD) <- mkSortableDataD prefix x
        let tyhead = conT n
        sequence
            [ pure sortableD
            , instanceD
                  (cxt [])
                  [t|Sortable $(tyhead)|]
#if MIN_VERSION_template_haskell(2,9,0)
                  [ tySynInstD ''MkSortable (tySynEqn [tyhead] (conT dty))
#else
                  [ tySynInstD ''MkSortable [tyhead] (conT dty)
#endif
                  , funD 'sortable (mkSortableImpl prefix x)
                  ]
            ]

mkSortableDataD prefix (DataD cx tyName [] KIND_ARG cons _) =
    (,) newname <$>
    dataD (pure cx) newname [] KIND_ARG (map (mkSortableCon prefix) cons) commonDerivClause
  where
    newname = sortedTyName prefix tyName
mkSortableDataD prefix (NewtypeD cx tyName [] KIND_ARG con _) =
    (,) newname <$>
    newtypeD (pure cx) newname [] KIND_ARG (mkSortableCon prefix con) commonDerivClause
  where
    newname = sortedTyName prefix tyName
mkSortableDataD _ x = error $ "Unhandled: mkSortableDataD " ++ show x

#if MIN_VERSION_template_haskell(2,11,0)
bangDef = bang noSourceUnpackedness noSourceStrictness
#else
bangDef = pure NotStrict
#endif

mkSortableCon prefix (RecC recName fields) =
    recC (sortedTyName prefix recName) (map mkSortedField fields)
  where
    mkSortedField (varname, _, varty) =
#if MIN_VERSION_template_haskell(2,11,0)
        varBangType
            (sortedValName varname)
            (bangType bangDef [t|MkSortable $(pure varty)|])
#else
        varStrictType
            (sortedValName varname)
            (strictType bangDef [t|MkSortable $(pure varty)|])
#endif
mkSortableCon prefix (NormalC nm tys) =
    normalC (sortedTyName prefix nm) (map mkSortedField tys)
  where
    mkSortedField (_, varty) =
#if MIN_VERSION_template_haskell(2,11,0)
        bangType bangDef [t|MkSortable $(pure varty)|]
#else
        strictType bangDef [t|MkSortable $(pure varty)|]
#endif
mkSortableCon _ x = error $ "Unhandled case in mkSortableCon: " ++ show x

sortedTyName pref = mkName . ("MkSort" ++) . (pref ++) . nameBase

sortedValName = mkName . ("mkSort" ++) . firstToUpper . nameBase
  where
    firstToUpper (x:xs) = toUpper x : xs
    firstToUpper [] = []

mkSortableImpl pref (DataD _ _ _ KIND_ARG cons _) = map (mkSortableImplClause pref) cons
mkSortableImpl pref (NewtypeD _ _ _ KIND_ARG con _) = [mkSortableImplClause pref con]
mkSortableImpl _ x = error $ "Unhandled: mkSortableImpl " ++ show x

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
