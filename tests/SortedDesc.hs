{-# Language NamedFieldPuns     #-}
{-# Language RecordWildCards    #-}
{-# Language StandaloneDeriving #-}
{-# Language TypeFamilies       #-}

module SortedDesc where

import           Data.Map                               (Map)
import           Data.Set                               (Set)
import qualified Data.Set                               as S
import           Distribution.Compiler
import           Distribution.License
import           Distribution.ModuleName
import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.Types.LegacyExeDependency
import           Distribution.Types.Mixin
import           Distribution.Types.PkgconfigDependency
import           Distribution.Types.UnqualComponentName
import           Distribution.Version
import           Language.Haskell.Extension

data SGenericPackageDescription = SGenericPackageDescription
    { packageDescription :: SPackageDescription
    , genPackageFlags    :: Set Flag
    -- , sCondLibrary :: Maybe (SCondTree SConfVar (Set SDependency) SLibrary)
    -- , SCondExecutables :: Set (String, SCondTree SConfVar (Set SDependency) SExecutable)
    }
    deriving (Eq, Show)

data SPackageDescription = SPackageDescription
    { package        :: PackageIdentifier
    , license        :: License
    , licenseFiles   :: Set FilePath
    , copyright      :: String
    , maintainer     :: String
    , author         :: String
    , stability      :: String
    , testedWith     :: Set (CompilerFlavor, VersionRange)
    , homepage       :: String
    , pkgUrl         :: String
    , bugReports     :: String
    , sourceRepos    :: Set SourceRepo
    , synopsis       :: String
    , description    :: String
    , category       :: String
    , customFieldsPD :: Set (String, String)
    , specVersionRaw :: Either Version VersionRange
    , buildType      :: Maybe BuildType
    , setupBuildInfo :: Maybe SSetupBuildInfo
    , library        :: Maybe SLibrary
    , executables    :: Set SExecutable
    , testSuites     :: Set STestSuite
    , benchmarks     :: Set SBenchmark
    , dataFiles      :: Set FilePath
    , dataDir        :: FilePath
    , extraSrcFiles  :: Set FilePath
    , extraTmpFiles  :: Set FilePath
    , extraDocFiles  :: Set FilePath
    }
    deriving (Show, Eq)

data SLibrary = SLibrary
    { exposedModules    :: Set ModuleName
    , reexportedModules :: Set ModuleReexport
    , signatures        :: Set ModuleName
    , libExposed        :: Bool
    , libBuildInfo      :: SBuildInfo
    }
    deriving (Show, Eq)

data SExecutable = SExecutable
    { exeName    :: UnqualComponentName
    , modulePath :: FilePath
    , buildInfo  :: SBuildInfo
    }
    deriving (Show, Eq, Ord)

data STestSuite = STestSuite
    { testName      :: UnqualComponentName
    , testInterface :: TestSuiteInterface
    , testBuildInfo :: SBuildInfo
    }
    deriving (Show, Eq, Ord)

data SBenchmark = SBenchmark
    { benchmarkName      :: UnqualComponentName
    , benchmarkInterface :: BenchmarkInterface
    , benchmarkBuildInfo :: SBuildInfo
    }
    deriving (Show, Eq, Ord)

data SSetupBuildInfo = SSetupBuildInfo
    { setupDepends        :: Set Dependency
    , defaultSetupDepends :: Bool
    }
    deriving (Show, Eq)

data SBuildInfo = SBuildInfo
    { buildable          :: Bool
    , buildTools         :: Set LegacyExeDependency
    , cppOptions         :: Set String
    , ccOptions          :: Set String
    , ldOptions          :: Set String
    , pkgconfigDepends   :: Set PkgconfigDependency
    , frameworks         :: Set String
    , extraFrameworkDirs :: Set String
    , cSources           :: Set FilePath
    , jsSources          :: Set FilePath
    , hsSourceDirs       :: Set FilePath
    , otherModules       :: Set ModuleName
    , defaultLanguage    :: Maybe Language
    , otherLanguages     :: Set Language
    , defaultExtensions  :: Set Extension
    , otherExtensions    :: Set Extension
    , oldExtensions      :: Set Extension
    , extraLibs          :: Set String
    , extraGHCiLibs      :: Set String
    , extraLibDirs       :: Set String
    , includeDirs        :: Set FilePath
    , includes           :: Set FilePath
    , installIncludes    :: Set FilePath
    , options            :: Set (CompilerFlavor, Set String)
    , profOptions        :: Set (CompilerFlavor, Set String)
    , sharedOptions      :: Set (CompilerFlavor, Set String)
    , customFieldsBI     :: Set (String, String)
    , targetBuildDepends :: Set Dependency
    , mixins             :: Set Mixin
    }
    deriving (Show, Eq, Ord)

class Convert a where
    type From a

    from :: From a -> a

instance Convert SGenericPackageDescription where
    type From SGenericPackageDescription = GenericPackageDescription

    from GenericPackageDescription{..} = SGenericPackageDescription
        { packageDescription = from packageDescription
        , genPackageFlags = S.fromList $ map (\ f -> f { flagDescription = "" }) genPackageFlags
        }

instance Convert SPackageDescription where
    type From SPackageDescription = PackageDescription

    from PackageDescription{..} = SPackageDescription
        { package
        , license
        , licenseFiles = S.fromList licenseFiles
        , copyright
        , maintainer
        , author
        , stability
        , testedWith = S.fromList testedWith
        , homepage
        , pkgUrl
        , bugReports
        , sourceRepos = S.fromList sourceRepos
        , synopsis
        , description = "" -- discard, matching formatting in tests is too much work
        , category
        , customFieldsPD = S.fromList customFieldsPD
        , specVersionRaw = Right anyVersion -- discard, cabal replaces * with >= 0 || <= 0
        , buildType
        , setupBuildInfo = from <$> setupBuildInfo
        , library = from <$> library
        , executables = S.fromList $ map from executables
        , testSuites = S.fromList $ map from testSuites
        , benchmarks = S.fromList $ map from benchmarks
        , dataFiles = S.fromList dataFiles
        , dataDir
        , extraSrcFiles = S.fromList extraSrcFiles
        , extraTmpFiles = S.fromList extraTmpFiles
        , extraDocFiles = S.fromList extraDocFiles
        }

instance Convert SSetupBuildInfo where
    type From SSetupBuildInfo = SetupBuildInfo

    from SetupBuildInfo{..} = SSetupBuildInfo
        { setupDepends = S.fromList setupDepends
        , defaultSetupDepends
        }

instance Convert SLibrary where
    type From SLibrary = Library

    from Library{..} = SLibrary
        { exposedModules = S.fromList exposedModules
        , reexportedModules = S.fromList reexportedModules
        , signatures = S.fromList signatures
        , libExposed
        , libBuildInfo = from libBuildInfo
        }

instance Convert SExecutable where
    type From SExecutable = Executable

    from Executable{..} = SExecutable
        { exeName
        , modulePath
        , buildInfo = from buildInfo
        }

instance Convert STestSuite where
    type From STestSuite = TestSuite

    from TestSuite{..} = STestSuite
        { testName
        , testInterface
        , testBuildInfo = from testBuildInfo
        }

instance Convert SBenchmark where
    type From SBenchmark = Benchmark

    from Benchmark{..} = SBenchmark
        { benchmarkName
        , benchmarkInterface
        , benchmarkBuildInfo = from benchmarkBuildInfo
        }

instance Convert SBuildInfo where
    type From SBuildInfo = BuildInfo

    from BuildInfo{..} = SBuildInfo
        { buildable
        , buildTools = S.fromList buildTools
        , cppOptions = S.fromList cppOptions
        , ccOptions = S.fromList ccOptions
        , ldOptions = S.fromList ldOptions
        , pkgconfigDepends = S.fromList pkgconfigDepends
        , frameworks = S.fromList frameworks
        , extraFrameworkDirs = S.fromList extraFrameworkDirs
        , cSources = S.fromList cSources
        , jsSources = S.fromList jsSources
        , hsSourceDirs = S.fromList hsSourceDirs
        , otherModules = S.fromList otherModules
        , defaultLanguage
        , otherLanguages = S.fromList otherLanguages
        , defaultExtensions = S.fromList defaultExtensions
        , otherExtensions = S.fromList otherExtensions
        , oldExtensions = S.fromList oldExtensions
        , extraLibs = S.fromList extraLibs
        , extraGHCiLibs = S.fromList extraGHCiLibs
        , extraLibDirs = S.fromList extraLibDirs
        , includeDirs = S.fromList includeDirs
        , includes = S.fromList includes
        , installIncludes = S.fromList installIncludes
        , options = S.fromList $ map (fmap S.fromList) options
        , profOptions = S.fromList $ map (fmap S.fromList) profOptions
        , sharedOptions = S.fromList $ map (fmap S.fromList) sharedOptions
        , customFieldsBI = S.fromList customFieldsBI
        , targetBuildDepends = S.fromList targetBuildDepends
        -- , targetBuildRenaming = targetBuildRenaming
        }

deriving instance Ord Flag
deriving instance Ord VersionRange
deriving instance Ord SourceRepo
deriving instance Ord Dependency
deriving instance Ord Language
deriving instance Ord ModuleReexport
deriving instance Ord TestSuiteInterface
deriving instance Ord TestType
deriving instance Ord BenchmarkInterface
deriving instance Ord BenchmarkType
deriving instance Ord PkgconfigDependency
deriving instance Ord LegacyExeDependency
