{-# Language RecordWildCards #-}

module Transform (toBlocks) where

import Data.Char
import Data.Maybe
import Distribution.License
import Distribution.PackageDescription
import Distribution.Types.CondTree
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
          , map libToBlock $ maybeToList condLibrary
          , map (uncurry exeToBlock) condExecutables
          , map (uncurry testToBlock) condTestSuites
          , map (uncurry benchToBlock) condBenchmarks
          ])

setupBuildInfoToBlock SetupBuildInfo {..} =
    Block
        CustomSetup
        [ nonEmpty (dependencies "setup-depends") setupDepends
        ]
        []


sourceRepoToBlock SourceRepo {..} =
    Block
        (SourceRepo_ repoKind)
        [ stringField "type" . showType =<< repoType
        , stringField "location" =<< repoLocation
        , file "subdir" =<< repoSubdir
        , file "tag" =<< repoTag
        , file "branch" =<< repoBranch
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
    , nonEmpty (stringField "author") author
    , nonEmpty (stringField "stability") stability
    , nonEmpty (testedField "tested-with") testedWith
    , nonEmpty (stringField "maintainer") maintainer
    , nonEmpty (stringField "copyright") copyright
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

libToBlock CondNode {..} =
    Block
        Library_
        (libDataToFields condTreeData ++ buildInfoToFields (libBuildInfo condTreeData))
        (nodesToBlocks libBuildInfo libDataToFields condTreeComponents)

libDataToFields Library {..} = [nonEmpty (modules "exposed-modules") exposedModules]

exeToBlock exeName CondNode {..} =
    Block
        (Exe_ (unUnqualComponentName exeName))
        (exeDataToFields condTreeData ++ buildInfoToFields (buildInfo condTreeData))
        (nodesToBlocks buildInfo exeDataToFields condTreeComponents)

exeDataToFields Executable {..} = [nonEmpty (stringField "main-is") modulePath]

testToBlock testName CondNode {..} =
    Block
        (TestSuite_ (unUnqualComponentName testName))
        (testDataToFields condTreeData ++ buildInfoToFields (testBuildInfo condTreeData))
        (nodesToBlocks testBuildInfo testDataToFields condTreeComponents)

testDataToFields TestSuite {..} =
    case testInterface of
        TestSuiteExeV10 _ f ->
            [stringField "type" "exitcode-stdio-1.0", stringField "main-is" f]
        TestSuiteLibV09 _ m ->
            [stringField "type" "detailed-0.9", module_ "test-module" m]
        _ -> []

benchToBlock benchName CondNode {..} =
    Block
        (Benchmark_ $ unUnqualComponentName benchName)
        (benchDataToFields condTreeData ++
         buildInfoToFields (benchmarkBuildInfo condTreeData))
        []

benchDataToFields Benchmark {..} =
    case benchmarkInterface of
        BenchmarkExeV10 _ f ->
            [stringField "type" "exitcode-stdio-1.0", stringField "main-is" f]
        _ -> []

nodesToBlocks f renderMore = concatMap (condNodeToBlock f renderMore)

condNodeToBlock getBuildInfo extra (CondBranch pred' branch1 branch2) =
    let b1 =
            Block
                (If pred')
                (extra (condTreeData branch1) ++
                 buildInfoToFields (getBuildInfo $ condTreeData branch1))
                (nodesToBlocks getBuildInfo extra (condTreeComponents branch1))
        b2 =
            branch2 <&> \b ->
                Block
                    Else
                    (extra (condTreeData b) ++
                     buildInfoToFields (getBuildInfo $ condTreeData b))
                    (nodesToBlocks getBuildInfo extra (condTreeComponents b))
     in b1 : maybeToList b2

buildInfoToFields BuildInfo {..} =
    [ nonEmpty (modules "other-modules") otherModules
    , nonEmpty (commas "hs-source-dirs") hsSourceDirs
    , stringField "default-language" . show =<< defaultLanguage
    , nonEmpty (extensions "default-extensions") defaultExtensions
    , buildDeps targetBuildDepends
    , nonEmpty (extensions "extensions") oldExtensions
    , nonEmpty (spaces "cpp-options") cppOptions
    , nonEmpty (commas "extra-libraries") extraLibs
    , nonEmpty (commas "frameworks") frameworks
    , nonEmpty (extensions "other-extensions") otherExtensions
    , ffilter not (stringField "buildable" . show) buildable
    ] ++
    map optionToField options ++
    [ nonEmpty (commas "c-sources") cSources
    , nonEmpty (commas "include-dirs") includeDirs
    ]

optionToField (f, args) = spaces (map toLower (show f) ++ "-options") args
