{-# Language CPP #-}
{-# Language DataKinds #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
{-# Language StandaloneDeriving #-}

#define PRIM(c) instance Shaped c where \
    type Shape c = Prim c; \
    project = projectPrim; \
    embed = embedPrim; \
    match = matchPrim; \
    render = prettyPrim

#define DERIVE(c) deriveGeneric ''c; instance Shaped c

module Instances where

import Data.ByteString.Short
import Data.Word
import Distribution.Compiler
import Control.DeepSeq
import qualified Distribution.SPDX as SPDX
import Distribution.License
import Distribution.ModuleName
import Distribution.Types.Benchmark
import Distribution.Types.Condition
import Distribution.System
import Distribution.Types.BenchmarkInterface
import Distribution.Types.BenchmarkType
import Distribution.Types.BuildInfo
import Distribution.Types.BuildType
import Distribution.Types.CondTree
import Distribution.Types.Dependency
import Distribution.Types.ExeDependency
import Distribution.Types.Executable
import Distribution.Types.ExecutableScope
import Distribution.Types.ForeignLib
import Distribution.Types.ForeignLibOption
import Distribution.Types.ForeignLibType
import Distribution.Types.GenericPackageDescription
import Distribution.Types.IncludeRenaming
import Distribution.Types.LegacyExeDependency
import Distribution.Types.Library
import Distribution.Types.Mixin
import Distribution.Types.ModuleReexport
import Distribution.Types.ModuleRenaming
import Distribution.Types.PackageDescription
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.PkgconfigDependency
import Distribution.Types.PkgconfigName
import Distribution.Types.SetupBuildInfo
import Distribution.Types.SourceRepo
import Distribution.Types.TestSuite
import Distribution.Types.TestSuiteInterface
import Distribution.Types.TestType
import Distribution.Types.UnqualComponentName
import Distribution.Utils.ShortText
import Distribution.Version
import Generics.SOP.TH
import Language.Haskell.Extension
import Prelude.Compat
import Test.StrictCheck.Instances
import Test.StrictCheck.Instances.Tools
import Test.StrictCheck.Shaped

DERIVE(Arch)
DERIVE(Benchmark)
DERIVE(BenchmarkInterface)
DERIVE(BenchmarkType)
DERIVE(BuildInfo)
DERIVE(BuildType)
DERIVE(CompilerFlavor)
DERIVE(ConfVar)
DERIVE(Dependency)
DERIVE(ExeDependency)
DERIVE(Executable)
DERIVE(ExecutableScope)
DERIVE(Extension)
DERIVE(Flag)
DERIVE(FlagName)
DERIVE(ForeignLib)
DERIVE(ForeignLibOption)
DERIVE(ForeignLibType)
DERIVE(GenericPackageDescription)
DERIVE(IncludeRenaming)
DERIVE(KnownExtension)
DERIVE(Language)
DERIVE(LegacyExeDependency)
DERIVE(LibVersionInfo)
DERIVE(Library)
DERIVE(License)
DERIVE(SPDX.License)
DERIVE(SPDX.LicenseExpression)
DERIVE(SPDX.SimpleLicenseExpression)
DERIVE(SPDX.LicenseId)
DERIVE(SPDX.LicenseRef)
DERIVE(SPDX.LicenseExceptionId)
DERIVE(Mixin)
DERIVE(ModuleReexport)
DERIVE(ModuleRenaming)
DERIVE(OS)
DERIVE(PackageDescription)
DERIVE(PackageIdentifier)
DERIVE(PackageName)
DERIVE(PkgconfigDependency)
DERIVE(PkgconfigName)
DERIVE(RepoKind)
DERIVE(RepoType)
DERIVE(SetupBuildInfo)
DERIVE(ShortText)
DERIVE(SourceRepo)
DERIVE(TestSuite)
DERIVE(TestSuiteInterface)
DERIVE(TestType)
DERIVE(UnqualComponentName)
DERIVE(Version)
DERIVE(VersionRange)

deriveGeneric ''CondTree

deriveGeneric ''CondBranch

deriveGeneric ''Condition

instance (Shaped a, Shaped b, Shaped c) => Shaped (CondTree a b c)

instance (Shaped a, Shaped b, Shaped c) => Shaped (CondBranch a b c)

instance Shaped a => Shaped (Condition a)

PRIM(Bool)
PRIM(Char)
PRIM(Int)
PRIM(ModuleName) -- contains ShortTextLst, which isn't exported
PRIM(ShortByteString)
PRIM(Word64)
