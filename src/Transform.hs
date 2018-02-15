{-# Language FlexibleContexts #-}
{-# Language RecordWildCards #-}

module Transform
    ( toBlocks
    ) where

import Control.Arrow
import Control.DeepSeq
import Data.Char
import Data.Either
import Data.Maybe
import Distribution.License
import Distribution.PackageDescription
import Distribution.Simple.BuildToolDepends
import Distribution.Types.CondTree
import Distribution.Types.ExecutableScope
import Distribution.Types.ForeignLib
import Distribution.Types.ForeignLibType
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.UnqualComponentName

import Types.Block
import Types.Field

(<&>) = flip fmap

toBlocks GenericPackageDescription {..} =
    ( pdToFields packageDescription
    , concat
          [ map setupBuildInfoToBlock $ maybeToList (setupBuildInfo packageDescription)
          , map sourceRepoToBlock (sourceRepos packageDescription)
          , map flagToBlock genPackageFlags
          , map (libToBlock packageDescription Nothing) $ maybeToList condLibrary
          , map (uncurry (libToBlock packageDescription) . first Just) condSubLibraries
          , map (uncurry $ foreignLibToBlock packageDescription) condForeignLibs
          , map (uncurry $ exeToBlock packageDescription) condExecutables
          , map (uncurry $ testToBlock packageDescription) condTestSuites
          , map (uncurry $ benchToBlock packageDescription) condBenchmarks
          ])

setupBuildInfoToBlock SetupBuildInfo {..}
    -- defaultSetupDepends doesn't correspond to anything in the cabal file
 =
    defaultSetupDepends `seq`
    Block CustomSetup [nonEmpty (dependencies "setup-depends") setupDepends] []

sourceRepoToBlock SourceRepo {..} =
    Block
        (SourceRepo_ repoKind)
        [ stringField "type" . showType =<< repoType
        , stringField "location" =<< repoLocation
        , file "subdir" =<< repoSubdir
        , file "tag" =<< repoTag
        , file "branch" =<< repoBranch
        , file "module" =<< repoModule
        ]
        []
  where
    showType (OtherRepoType n) = n
    showType x = map toLower $ show x

pdToFields pd@PackageDescription {..} =
    [ stringField "name" (unPackageName $ pkgName package)
    , version "version" (pkgVersion package)
    , nonEmpty (stringField "synopsis") synopsis
    , desc description
    , ffilter (/= UnspecifiedLicense) (licenseField "license") license
    , license' licenseFiles
    , nonEmpty (stringField "copyright") copyright
    , nonEmpty (stringField "author") author
    , nonEmpty (stringField "stability") stability
    , nonEmpty (testedField "tested-with") testedWith
    , nonEmpty (stringField "maintainer") maintainer
    , nonEmpty (stringField "category") category
    , nonEmpty (stringField "homepage") homepage
    , nonEmpty (stringField "package-url") pkgUrl
    , nonEmpty (stringField "bug-reports") bugReports
    , stringField "build-type" . show =<< buildType
    , nonEmpty (longList "extra-tmp-files") extraTmpFiles
    , nonEmpty (longList "extra-source-files") extraSrcFiles
    , nonEmpty (longList "extra-doc-files") extraDocFiles
    , nonEmpty (longList "data-files") dataFiles
    , nonEmpty (stringField "data-dir") dataDir
    , cabalVersion "cabal-version" (specVersion pd)
    ] ++
    map (uncurry stringField) customFieldsPD
  where
    license' [] = Nothing
    license' [l] = file "license-file" l
    license' ls = commas "license-files" ls

flagToBlock MkFlag {..} =
    Block
        (Flag_ (unFlagName flagName))
        [ stringField "default" (show flagDefault)
        , ffilter id (stringField "manual" . show) flagManual
        , desc flagDescription
        ]
        []

libToBlock pkg libname CondNode {..} =
    deepseq condTreeConstraints $
    Block
        (Library_ $ unUnqualComponentName <$> libname)
        (libDataToFields condTreeData ++ buildInfoToFields pkg (libBuildInfo condTreeData))
        (nodesToBlocks pkg libBuildInfo libDataToFields condTreeComponents)

libDataToFields Library {..} =
    libName `seq`
    [ ffilter not (stringField "exposed" . show) libExposed
    , nonEmpty (modules "exposed-modules") exposedModules
    , nonEmpty (rexpModules "reexported-modules") reexportedModules
    , nonEmpty (modules "signatures") signatures
    ]

foreignLibToBlock pkg libname CondNode {..} =
    deepseq condTreeConstraints $
    Block
        (ForeignLib_ $ unUnqualComponentName libname)
        (foreignLibDataToFields condTreeData ++
         buildInfoToFields pkg (foreignLibBuildInfo condTreeData))
        (nodesToBlocks pkg foreignLibBuildInfo foreignLibDataToFields condTreeComponents)

foreignLibDataToFields ForeignLib {..} =
    foreignLibName `seq`
    [ ffilter (== ForeignLibNativeShared) (flibType "type") foreignLibType
    , nonEmpty (flibOptions "options") foreignLibOptions
    , stringField "lib-version-info" . showLVI =<< foreignLibVersionInfo
    , version "lib-version-linux" =<< foreignLibVersionLinux
    , nonEmpty (commas "mod-def-file") foreignLibModDefFile
    ]
  where
    showLVI l =
        let (a, b, c) = libVersionInfoCRA l
         in show a ++ ":" ++ show b ++ ":" ++ show c

exeToBlock pkg exeName CondNode {..} =
    deepseq condTreeConstraints $
    Block
        (Exe_ (unUnqualComponentName exeName))
        (exeDataToFields condTreeData ++ buildInfoToFields pkg (buildInfo condTreeData))
        (nodesToBlocks pkg buildInfo exeDataToFields condTreeComponents)

exeDataToFields Executable {..} =
    exeName `seq`
    [ nonEmpty (stringField "main-is") modulePath
    , ffilter (== ExecutablePrivate) (\_ -> stringField "scope" "private") exeScope
    ]

testToBlock pkg testName CondNode {..} =
    deepseq condTreeConstraints $
    Block
        (TestSuite_ (unUnqualComponentName testName))
        (testDataToFields condTreeData ++
         buildInfoToFields pkg (testBuildInfo condTreeData))
        (nodesToBlocks pkg testBuildInfo testDataToFields condTreeComponents)

testDataToFields TestSuite {..} =
    testName `seq`
    case testInterface of
        TestSuiteExeV10 v f ->
            v `seq` [stringField "type" "exitcode-stdio-1.0", stringField "main-is" f]
        TestSuiteLibV09 v m ->
            v `seq` [stringField "type" "detailed-0.9", module_ "test-module" m]
        _ -> []

benchToBlock pkg benchName CondNode {..} =
    deepseq condTreeConstraints $
    Block
        (Benchmark_ $ unUnqualComponentName benchName)
        (benchDataToFields condTreeData ++
         buildInfoToFields pkg (benchmarkBuildInfo condTreeData))
        (nodesToBlocks pkg benchmarkBuildInfo benchDataToFields condTreeComponents)

benchDataToFields Benchmark {..} =
    benchmarkName `seq`
    case benchmarkInterface of
        BenchmarkExeV10 v f ->
            v `seq` [stringField "type" "exitcode-stdio-1.0", stringField "main-is" f]
        _ -> []

nodesToBlocks pkg f renderMore = concatMap (condNodeToBlock pkg f renderMore)

condNodeToBlock pkg getBuildInfo extra (CondBranch pred' branch1 branch2) =
    let b1 =
            deepseq (condTreeConstraints branch1) $
            Block
                (If pred')
                (extra (condTreeData branch1) ++
                 buildInfoToFields pkg (getBuildInfo $ condTreeData branch1))
                (nodesToBlocks pkg getBuildInfo extra (condTreeComponents branch1))
        b2 =
            branch2 <&> \b ->
                deepseq (condTreeConstraints b) $
                Block
                    Else
                    (extra (condTreeData b) ++
                     buildInfoToFields pkg (getBuildInfo $ condTreeData b))
                    (nodesToBlocks pkg getBuildInfo extra (condTreeComponents b))
     in b1 : maybeToList b2

buildInfoToFields pkg BuildInfo {..} =
    [ nonEmpty (commas "other-languages" . map show) otherLanguages
    , nonEmpty (modules "other-modules") otherModules
    , nonEmpty (mixins_ "mixins") mixins
    , nonEmpty (modules "autogen-modules") autogenModules
    , nonEmpty (commas "hs-source-dirs") hsSourceDirs
    , buildDeps targetBuildDepends
    , stringField "default-language" . show =<< defaultLanguage
    , nonEmpty (extensions "default-extensions") defaultExtensions
    , nonEmpty (extensions "extensions") oldExtensions
    , nonEmpty (extensions "other-extensions") otherExtensions
    , nonEmpty (commas "extra-libraries") extraLibs
    , nonEmpty (commas "extra-ghci-libraries") extraGHCiLibs
    , nonEmpty (pcDepends "pkgconfig-depends") pkgconfigDepends
    , nonEmpty (commas "frameworks") frameworks
    , nonEmpty (commas "extra-framework-dirs") extraFrameworkDirs
    , nonEmpty (spaces "cpp-options") cppOptions
    , nonEmpty (spaces "cc-options") ccOptions
    , nonEmpty (spaces "ld-options") ldOptions
    , nonEmpty (commas "c-sources") cSources
    , nonEmpty (commas "extra-lib-dirs") extraLibDirs
    , nonEmpty (commas "includes") includes
    , nonEmpty (commas "install-includes") installIncludes
    , nonEmpty (commas "include-dirs") includeDirs
    , nonEmpty (commas "js-sources") jsSources
    , ffilter not (stringField "buildable" . show) buildable
    , nonEmpty (toolDepends "build-tool-depends") newTools
    , nonEmpty (oldToolDepends "build-tools") oldTools
    ] ++
    map (optionToField "") options ++
    map (optionToField "-prof") profOptions ++
    map (optionToField "-shared") sharedOptions ++
    map (uncurry stringField) customFieldsBI
  where
    (oldTools, newTools) =
        partitionEithers $
        map Right buildToolDepends ++
        map
            (\dep ->
                 case desugarBuildTool pkg dep of
                     Just k -> Right k
                     Nothing -> Left dep)
            buildTools

optionToField pref (f, args) = spaces (map toLower (show f) ++ pref ++ "-options") args
