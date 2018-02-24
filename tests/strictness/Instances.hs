{-# Language CPP #-}
{-# Language DataKinds #-}
{-# Language DeriveAnyClass #-}
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
#define DERIVEN(c) DERIVE(c); deriving instance NFData c

module Instances where

import Data.ByteString.Short
import Data.Word
import Distribution.Compiler
import Control.DeepSeq
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
DERIVE(BuildType)
DERIVE(ConfVar)
DERIVE(Dependency)
DERIVE(ExeDependency)
DERIVE(Flag)
DERIVE(FlagName)
DERIVE(GenericPackageDescription)
DERIVE(LegacyExeDependency)
DERIVE(License)
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
DERIVE(UnqualComponentName)
DERIVE(Version)
DERIVE(VersionRange)

DERIVEN(Benchmark)
DERIVEN(BenchmarkInterface)
DERIVEN(BenchmarkType)
DERIVEN(BuildInfo)
DERIVEN(CompilerFlavor)
DERIVEN(Executable)
DERIVEN(ExecutableScope)
DERIVEN(Extension)
DERIVEN(ForeignLib)
DERIVEN(ForeignLibOption)
DERIVEN(ForeignLibType)
DERIVEN(IncludeRenaming)
DERIVEN(KnownExtension)
DERIVEN(Language)
DERIVEN(LibVersionInfo)
DERIVEN(Library)
DERIVEN(Mixin)
DERIVEN(ModuleReexport)
DERIVEN(ModuleRenaming)
DERIVEN(TestSuite)
DERIVEN(TestSuiteInterface)
DERIVEN(TestType)

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
