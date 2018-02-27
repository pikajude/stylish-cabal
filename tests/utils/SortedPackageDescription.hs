{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
{-# Language NamedFieldPuns #-}
{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell #-}
{-# Language TypeApplications #-}
{-# Language RecordWildCards #-}
{-# Language StandaloneDeriving #-}
{-# Language TypeFamilies #-}

module SortedPackageDescription
    ( Sortable(..)
    , sortGenericPackageDescription
    , MkSortGenericPackageDescription(..)
    ) where

import Data.Char (isSpace)
import Data.Word
import Distribution.Compiler
import Distribution.License
import Distribution.ModuleName
import Distribution.PackageDescription
import Distribution.System
import Distribution.Types.CondTree
import Distribution.Types.Dependency
import Distribution.Types.ExeDependency
import Distribution.Types.ExecutableScope
import Distribution.Types.ForeignLib
import Distribution.Types.ForeignLibOption
import Distribution.Types.ForeignLibType
import Distribution.Types.IncludeRenaming
import Distribution.Types.LegacyExeDependency
import Distribution.Types.Mixin
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.PkgconfigDependency
import Distribution.Types.PkgconfigName
import Distribution.Types.UnqualComponentName
import Distribution.Utils.ShortText
import Distribution.Version
import Language.Haskell.Extension
import Prelude.Compat
import SortedPackageDescription.TH

sortGenericPackageDescription = sortable @GenericPackageDescription . squashDescription
  where
    squashDescription g@GenericPackageDescription {..} =
        g
            { packageDescription = squashPD packageDescription
            , genPackageFlags = map squashFlag genPackageFlags
            }
    squashPD p@PackageDescription {..} =
        p {description = squash description, synopsis = squash synopsis}
    squashFlag f@MkFlag {..} = f {flagDescription = squash flagDescription}
    squash = filter (not . isSpace)

prim [''ModuleName, ''ShortText, ''Char, ''Word64, ''PackageName, ''Int, ''Bool]

deriveSortable
    [ ''BuildType
    , ''Language
    , ''Version
    , ''VersionRange
    , ''ModuleReexport
    , ''Dependency
    , ''SetupBuildInfo
    , ''UnqualComponentName
    , ''LegacyExeDependency
    , ''PkgconfigName
    , ''PkgconfigDependency
    , ''ExeDependency
    , ''KnownExtension
    , ''Extension
    , ''OS
    , ''Arch
    , ''FlagName
    , ''CompilerFlavor
    , ''ModuleRenaming
    , ''IncludeRenaming
    , ''Mixin
    , ''BuildInfo
    , ''Library
    , ''ExecutableScope
    , ''Executable
    , ''License
    , ''ConfVar
    , ''PackageIdentifier
    , ''RepoType
    , ''RepoKind
    , ''SourceRepo
    , ''PackageDescription
    , ''Flag
    , ''ForeignLib
    , ''ForeignLibType
    , ''ForeignLibOption
    , ''LibVersionInfo
    , ''TestSuite
    , ''TestSuiteInterface
    , ''TestType
    , ''Benchmark
    , ''BenchmarkInterface
    , ''BenchmarkType
    ]

deriving instance Ord SourceRepo

deriving instance (Ord a, Ord b, Ord c) => Ord (CondTree a b c)

deriving instance (Ord a, Ord b, Ord c) => Ord (CondBranch a b c)

deriving instance Ord a => Ord (Condition a)

deriving instance Ord Flag

deriving instance Ord Dependency

deriving instance Ord VersionRange

deriving instance Ord ConfVar

deriving instance Ord Library

deriving instance Ord ModuleReexport

deriving instance Ord BuildInfo

deriving instance Ord LegacyExeDependency

deriving instance Ord ExeDependency

deriving instance Ord PkgconfigDependency

deriving instance Ord Language

deriving instance Ord ForeignLib

deriving instance Ord ForeignLibType

deriving instance Ord ForeignLibOption

deriving instance Ord Executable

deriving instance Ord ExecutableScope

deriving instance Ord TestSuite

deriving instance Ord TestSuiteInterface

deriving instance Ord TestType

deriving instance Ord Benchmark

deriving instance Ord BenchmarkInterface

deriving instance Ord BenchmarkType

---------------------------------------------------------------------
-- everything below this line is copy/pasted from TH class generation
-- i am insufficiently intelligent to generate the correct instance heads
-- in TH, so i've done it manually
---------------------------------------------------------------------
data MkSortCondition c
    = MkSortVar (MkSortable c)
    | MkSortLit (MkSortable Bool)
    | MkSortCNot (MkSortable (Condition c))
    | MkSortCOr (MkSortable (Condition c))
                (MkSortable (Condition c))
    | MkSortCAnd (MkSortable (Condition c))
                 (MkSortable (Condition c))

instance Sortable a => Sortable (Condition a) where
    type MkSortable (Condition a) = MkSortCondition a
    sortable (Var arg) = MkSortVar (sortable arg)
    sortable (Lit arg) = MkSortLit (sortable arg)
    sortable (CNot arg) = MkSortCNot (sortable arg)
    sortable (COr arg arg2) = MkSortCOr (sortable arg) (sortable arg2)
    sortable (CAnd arg arg2) = MkSortCAnd (sortable arg) (sortable arg2)

data MkSortCondTree v c a = MkSortCondNode
    { mkSortCondTreeData :: MkSortable a
    , mkSortCondTreeConstraints :: MkSortable c
    , mkSortCondTreeComponents :: MkSortable [CondBranch v c a]
    }

deriving instance
         (Show (MkSortable v), Show (MkSortable c), Show (MkSortable a)) =>
         Show (MkSortCondTree v c a)

deriving instance
         (Eq (MkSortable c), Eq (MkSortable v), Eq (MkSortable a)) =>
         Eq (MkSortCondTree v c a)

deriving instance
         (Ord (MkSortable a), Ord (MkSortable v), Sortable c,
          Ord (MkSortable c)) =>
         Ord (MkSortCondTree v c a)

deriving instance Show (MkSortable v) => Show (MkSortCondition v)

deriving instance Eq (MkSortable a) => Eq (MkSortCondition a)

deriving instance Ord (MkSortable a) => Ord (MkSortCondition a)

deriving instance
         (Show (MkSortable v), Show (MkSortable c), Show (MkSortable a)) =>
         Show (MkSortCondBranch v c a)

deriving instance
         (Eq (MkSortable b), Eq (MkSortable a), Eq (MkSortable c)) =>
         Eq (MkSortCondBranch a b c)

deriving instance
         (Ord (MkSortable a), Ord (MkSortable b), Ord (MkSortable c),
          Sortable b) =>
         Ord (MkSortCondBranch a b c)

instance ( Sortable a
         , Sortable b
         , Sortable c
         , Ord (MkSortable a)
         , Ord (MkSortable b)
         , Ord (MkSortable c)
         ) =>
         Sortable (CondTree a b c) where
    type MkSortable (CondTree a b c) = MkSortCondTree a b c
    sortable (CondNode arg arg2 arg3) =
        MkSortCondNode (sortable arg) (sortable arg2) (sortable arg3)

data MkSortCondBranch v c a = MkSortCondBranch
    { mkSortCondBranchCondition :: MkSortable (Condition v)
    , mkSortCondBranchIfTrue :: MkSortable (CondTree v c a)
    , mkSortCondBranchIfFalse :: MkSortable (Maybe (CondTree v c a))
    }

instance ( Sortable a
         , Sortable b
         , Sortable c
         , Ord (MkSortable a)
         , Ord (MkSortable b)
         , Ord (MkSortable c)
         ) =>
         Sortable (CondBranch a b c) where
    type MkSortable (CondBranch a b c) = MkSortCondBranch a b c
    sortable (CondBranch arg arg2 arg3) =
        MkSortCondBranch (sortable arg) (sortable arg2) (sortable arg3)

deriveSortable [''GenericPackageDescription]
