{-# Language FlexibleContexts #-}
{-# Language NoMonomorphismRestriction #-}
{-# Language RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module StylishCabal
    ( prettify
    , FormatOpts(..)
    ) where

import Control.Monad.Reader
import Debug.Trace
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Monoid ((<>))
import Distribution.License
import Distribution.ModuleName (ModuleName, components)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.ParseUtils
import Distribution.Simple.Utils
import Distribution.Types.CondTree
import Distribution.Types.UnqualComponentName
import Distribution.Verbosity
import Distribution.Version
import Field
import Language.Haskell.Extension
import Text.PrettyPrint.Leijen hiding ((<$>), (<>))
import qualified Text.PrettyPrint.Leijen as L

instance Monoid Doc where
    mempty = L.empty
    mappend = (L.<>)

prettify :: String -> FormatOpts -> IO String
prettify input f =
    (`runReaderT` f) $ do
        gpd <-
            do let res = parseGenericPackageDescription input
               case res of
                   ParseFailed e -> do
                       let (line', message) = locatedErrorMsg e
                       liftIO $ dieWithLocation' normal "<input>" line' message
                   ParseOk warnings x -> do
                       mapM_ (liftIO . warn normal . showPWarning "<input>") $
                           reverse warnings
                       return x
        let pd = packageDescription gpd
        header <- renderFields =<< renderPackageDesc pd
        let blocks' =
                concat
                    [ map renderSourceRepo (sourceRepos pd)
                    , map renderFlag (genPackageFlags gpd)
                    , maybeToList $ renderLibrary <$> condLibrary gpd
                    , map (uncurry renderExe) (condExecutables gpd)
                    , map (uncurry renderTest) (condTestSuites gpd)
                    , map (uncurry renderBench) (condBenchmarks gpd)
                    , maybeToList $ renderSetupBuildInfo <$> setupBuildInfo pd
                    ]
        blocks <- mapM render =<< sequence blocks'
        let doc = vcat . intersperse L.empty $ header : blocks
        let formatted = displayS (renderPretty 1.0 (pageWidth f) $ doc <> line) ""
        return $ unlines $ map stripWs $ lines formatted
  where
    stripWs = reverse . dropWhile isSpace . reverse

renderSetupBuildInfo :: SetupBuildInfo -> StyleM Block
renderSetupBuildInfo SetupBuildInfo {..} =
    mkBlock
        (string "custom-setup")
        [ mkField
              "setup-depends"
              setupDepends
              (not . null)
              (tokens . sort . map showDependency)
        ]

renderSourceRepo :: SourceRepo -> StyleM Block
renderSourceRepo SourceRepo {..} =
    mkBlock
        (string "source-repository" <+> showKind repoKind)
        [ maybeField "type" repoType (string . showType)
        , maybeField "location" repoLocation string
        , maybeField "subdir" repoSubdir filepathToDoc
        , maybeField "tag" repoTag filepathToDoc
        , maybeField "branch" repoBranch filepathToDoc
        ]
  where
    showKind RepoHead = string "head"
    showKind RepoThis = string "this"
    showKind x = error $ show x
    showType (OtherRepoType n) = n
    showType x = map toLower $ show x

renderPackageDesc :: PackageDescription -> StyleM [Thing]
renderPackageDesc pd@PackageDescription {..} =
    sequence
        [ mkField' "name" (pkgName package) (string . unPackageName)
        , mkField' "version" (pkgVersion package) (string . showVersion)
        , mkNonempty "synopsis" synopsis string
        , longField
              "description"
              (normalizeDescription description)
              (const $ not $ null description)
        , mkField "license" license (/= UnspecifiedLicense) (string . showLicense)
        , pure $ ThingF $ license' licenseFiles
        , mkNonempty "author" author string
        , mkNonempty "stability" stability string
        , mkNonempty "tested-with" testedWith renderTestedWith
        , mkNonempty "maintainer" maintainer string
        , mkNonempty "copyright" copyright string
        , mkNonempty "category" category string
        , mkNonempty "homepage" homepage string
        , mkNonempty "package-url" pkgUrl string
        , mkNonempty "bug-reports" bugReports string
        , maybeField "build-type" buildType (string . show)
        , mkNonempty "extra-tmp-files" extraTmpFiles files
        , mkNonempty "extra-source-files" extraSrcFiles files
        , mkNonempty "extra-doc-files" extraDocFiles files
        , mkNonempty "data-files" dataFiles files
        , mkNonempty "data-dir" dataDir string
        , mkField'
              "cabal-version"
              (specVersion pd)
              (\v -> string ">=" <> string (showVersion v))
        ] <++>
    mapM (\(f, v) -> mkField' f v string) customFieldsPD
  where
    license' [] = noField
    license' [l] = Field "license-file" True (Left $ filepathToDoc l)
    license' ls = Field "license-files" True (Left $ tokens ls)

normalizeDescription :: String -> Doc
normalizeDescription str = desc
  where
    chunks'' = splitOn "\n\n" str
    chunks' =
        map
            (strip .
             map
                 (\c ->
                      if c == '\n'
                          then ' '
                          else c))
            chunks''
    strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse
    chunks = map (fillSep . map text . words) chunks'
    desc = vsep $ intersperse (string ".") chunks

renderTestedWith :: Show a => [(a, VersionRange)] -> Doc
renderTestedWith =
    fillSep .
    punctuate comma .
    map (\(compiler, vers) -> string $ showVersioned' (show compiler, vers))

renderNodes ::
       Traversable t
    => (a -> BuildInfo)
    -> (a -> StyleM [Thing])
    -> t (CondBranch ConfVar c a)
    -> StyleM [Thing]
renderNodes f renderMore branches = concat <$> mapM (renderCondNode f renderMore) branches

renderExe :: UnqualComponentName -> CondTree ConfVar c Executable -> StyleM Block
renderExe exeName CondNode {..} =
    Block (string "executable" <+> string (unUnqualComponentName exeName)) <$>
    (do cd <- renderExeData condTreeData
        bi <- showBuildInfo (buildInfo condTreeData)
        ns <- renderNodes buildInfo renderExeData condTreeComponents
        return $ cd ++ bi ++ ns)

renderLibrary :: Show c => CondTree ConfVar c Library -> StyleM Block
renderLibrary CondNode {..} =
    Block (string "library") <$>
    (do cd <- renderLibData condTreeData
        bi <- showBuildInfo (libBuildInfo condTreeData)
        ns <- renderNodes libBuildInfo renderLibData condTreeComponents
        return $ cd ++ bi ++ ns)

renderTest :: UnqualComponentName -> CondTree ConfVar c TestSuite -> StyleM Block
renderTest testName CondNode {..} =
    Block (string "test-suite" <+> string (unUnqualComponentName testName)) <$>
    (do cd <- renderTestData condTreeData
        bi <- showBuildInfo (testBuildInfo condTreeData)
        ns <- renderNodes testBuildInfo renderTestData condTreeComponents
        return $ cd ++ bi ++ ns)

renderBench benchName CondNode {..} =
    Block (string "benchmark" <+> string (unUnqualComponentName benchName)) <$>
    (do cd <- renderBenchData condTreeData
        bi <- showBuildInfo (benchmarkBuildInfo condTreeData)
        ns <- renderNodes benchmarkBuildInfo renderBenchData condTreeComponents
        return $ cd ++ bi ++ ns)

renderExeData :: Executable -> StyleM [Thing]
renderExeData Executable {..} = sequence [mkNonempty "main-is" modulePath string]

renderLibData :: Library -> StyleM [Thing]
renderLibData Library {..} =
    sequence [mkNonempty "exposed-modules" exposedModules modules']

renderTestData :: TestSuite -> StyleM [Thing]
renderTestData TestSuite {..} =
    sequence $
    case testInterface of
        TestSuiteExeV10 _ f ->
            [mkField' "type" "exitcode-stdio-1.0" string, mkField' "main-is" f string]
        TestSuiteLibV09 _ m ->
            [ mkField' "type" "detailed-0.9" string
            , mkField' "test-module" m (string . intercalate "." . components)
            ]
        TestSuiteUnsupported _ -> []

renderBenchData Benchmark {..} =
    sequence $
    case benchmarkInterface of
        BenchmarkExeV10 _ f ->
            [mkField' "type" "exitcode-stdio-1.0" string, mkField' "main-is" f string]
        BenchmarkUnsupported _ -> []

renderCondNode ::
       (a -> BuildInfo)
    -> (a -> StyleM [Thing])
    -> CondBranch ConfVar c a
    -> StyleM [Thing]
renderCondNode getBuildInfo extra (CondBranch pred' branch1 branch2) = do
    b1 <-
        Block (string "if" <+> showPredicate pred') <$>
        (do b <- extra (condTreeData branch1)
            bi <- showBuildInfo (getBuildInfo $ condTreeData branch1)
            ns <- renderNodes getBuildInfo extra (condTreeComponents branch1)
            return $ b ++ bi ++ ns)
    b2 <-
        case branch2 of
            Nothing -> pure Nothing
            Just b2 ->
                fmap Just $
                Block (string "else") <$>
                (do b <- extra (condTreeData b2)
                    bi <- showBuildInfo (getBuildInfo $ condTreeData b2)
                    ns <- renderNodes getBuildInfo extra (condTreeComponents b2)
                    return $ b ++ bi ++ ns)
    return $ map ThingB $ b1 : maybeToList b2

(<++>) :: Monad m => m [a] -> m [a] -> m [a]
(<++>) = liftM2 (++)

showPredicate :: Condition ConfVar -> Doc
showPredicate (Var x) = showVar x
showPredicate (CNot p) = string "!" <> showPredicate p
showPredicate (CAnd a b) = showPredicate a <+> string "&&" <+> showPredicate b
showPredicate (COr a b) = showPredicate a <+> string "||" <+> showPredicate b
showPredicate (Lit b) = string $ show b

showVar :: ConfVar -> Doc
showVar (Impl compiler vers) =
    string "impl" <> parens (string $ showVersioned' (map toLower $ show compiler, vers))
showVar (Flag f) = string "flag" <> parens (string (unFlagName f))
showVar (OS w) = string "os" <> parens (string $ map toLower $ show w)
showVar (Arch a) = string "arch" <> parens (string $ map toLower $ show a)

showBuildInfo :: BuildInfo -> StyleM [Thing]
showBuildInfo BuildInfo {..} = do
    ds <- sequence defaults
    if any (\(ThingF f) -> fFilter f) ds
        then return ds
        -- hack: rarely if ever do we want to render an empty build-info
        -- the "buildable" attribute is always present and nearly always True,
        -- but that would result in it showing up in every condtree
        --
        -- if build-info is empty, show buildable: True
        else sequence [mkField' "buildable" buildable (string . show)]
  where
    defaults =
        [ mkNonempty "other-modules" otherModules modules'
        , mkNonempty "js-sources" jsSources files
        , mkNonempty "hs-source-dirs" hsSourceDirs files
        , maybeField "default-language" defaultLanguage (string . show)
        , mkNonempty
              "default-extensions"
              defaultExtensions
              (tokens . sort . map showExtension . sort)
        , buildDependsField targetBuildDepends
        , mkNonempty "extensions" oldExtensions (tokens . sort . map showExtension . sort)
        , mkNonempty "cpp-options" cppOptions tokens'
        , mkNonempty "extra-libraries" extraLibs tokens
        , mkNonempty "frameworks" frameworks tokens
        , mkNonempty "other-extensions" otherExtensions (modules showExtension)
        , mkField "buildable" (string "False") (\_ -> not buildable) id
        ] ++
        map renderOption options ++
        [ mkNonempty "c-sources" cSources files
        , mkNonempty "include-dirs" includeDirs files
        ]

renderOption :: Show a => (a, [String]) -> StyleM Thing
renderOption (f, args) = mkField' (map toLower (show f) ++ "-options") args tokens'

modules' :: [ModuleName] -> Doc
modules' = modules (intercalate "." . components)

modules :: (a -> String) -> [a] -> Doc
modules f mnames = align $ vcat $ map string $ sort $ map f mnames

-- showOptions
tokens :: [String] -> Doc
tokens = align . vcat . punctuate comma . map string

tokens' :: [String] -> Doc
tokens' = fillSep . map string

files :: [String] -> Doc
files = fillSep . punctuate comma . map filepathToDoc

filepathToDoc :: String -> Doc
filepathToDoc x
    | null x = string "\"\""
    | any isSpace x = string $ show x
    | otherwise = string x

showDependency :: Dependency -> String
showDependency (Dependency pn v) = showVersioned' (unPackageName pn, v)

showVersioned' = either id (\(x, y) -> x ++ " " ++ y) . showVersioned

showVersioned :: (String, VersionRange) -> Either String (String, String)
showVersioned (pn, v')
    | v' == anyVersion = Left pn
    | otherwise =
        Right
            ( pn
            , foldVersionRange'
                  ""
                  (\v -> "== " ++ showVersion v)
                  (\v -> "> " ++ showVersion v)
                  (\v -> "< " ++ showVersion v)
                  (\v -> ">= " ++ showVersion v)
                  (\v -> "<= " ++ showVersion v)
                  (\v _ -> "== " ++ showVersion v ++ ".*")
                  (\v _ -> "^>= " ++ showVersion v)
                  (\a b -> a ++ " || " ++ b)
                  (\a b -> a ++ " && " ++ b)
                  (\a -> "(" ++ a ++ ")")
                  v')

showExtension :: Extension -> String
showExtension (EnableExtension s) = show s
showExtension (DisableExtension s) = "No" ++ show s
showExtension x = error $ show x

instance Ord Dependency where
    compare (Dependency p _) _
        | unPackageName p == "base" = LT
    compare _ (Dependency p _)
        | unPackageName p == "base" = GT
    compare (Dependency d1 _) (Dependency d2 _) = compare d1 d2

renderFlag :: Flag -> StyleM Block
renderFlag MkFlag {..} =
    mkBlock
        (string "flag" <+> string fname)
        [ mkField' "default" flagDefault (string . show)
        , mkField "manual" (string "True") (const flagManual) id
        , longField
              "description"
              (normalizeDescription flagDescription)
              (const $ not $ null flagDescription)
        ]
  where
    fname = unFlagName flagName

showLicense :: License -> String
showLicense MIT = "MIT"
showLicense BSD2 = "BSD2"
showLicense BSD3 = "BSD3"
showLicense BSD4 = "BSD4"
showLicense PublicDomain = "PublicDomain"
showLicense ISC = "ISC"
showLicense (MPL v) = showL "MPL" (Just v)
showLicense (LGPL v) = showL "LGPL" v
showLicense (GPL v) = showL "GPL" v
showLicense (AGPL v) = showL "AGPL" v
showLicense (Apache v) = showL "Apache" v
showLicense OtherLicense = "OtherLicense"
showLicense x = error $ show x

showL :: String -> Maybe Version -> String
showL s Nothing = s
showL s (Just v) = s ++ "-" ++ showVersion v

buildDependsField deps' = do
    pw <- asks pageWidth
    return $
        ThingF $
        Field "build-depends" (not $ null deps) $
        Right $ \width' key -> traceShow (short, width') $
            if length (show short) + width' <= pw
                then fill width' key <> align short
                else fill (width' - 2) key <> align long
  where
    deps = sort deps'
    len = maximum $ map (\(Dependency pn _) -> length $ unPackageName pn) deps
    short = cat $ punctuate (comma <> space) (map (string . showDependency) deps)
    long =
        encloseSep (string ": ") empty (comma <+> empty) $
        map
            (\(Dependency pn v) ->
                 either
                     string
                     (\(x, y) -> fill len (string x) <+> string y)
                     (showVersioned (unPackageName pn, v)))
            deps
