{-# Language NoMonomorphismRestriction #-}
{-# Language RecordWildCards           #-}

module StylishCabal where

import           Control.Monad
import           Data.Char
import           Data.IORef
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Monoid                            ((<>))
import           Data.Ord
import           Data.Version                           hiding (showVersion)
import           Debug.Trace
import           Distribution.License
import           Distribution.ModuleName                (components)
import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           Distribution.ParseUtils
import           Distribution.Simple.Utils
import           Distribution.Types.CondTree
import           Distribution.Types.UnqualComponentName
import           Distribution.Verbosity
import           Distribution.Version
import           Field
import           Field
import           Language.Haskell.Extension
import           System.Environment
import           System.IO
import           Text.PrettyPrint.Leijen                hiding ((<$>), (<>))
import qualified Text.PrettyPrint.Leijen                as L

instance Monoid Doc where
    mempty = L.empty
    mappend = (L.<>)

prettify :: String -> Int -> IO String
prettify input width = do
    writeIORef wideness width
    gpd <- do
        let res = parseGenericPackageDescription input
        case res of
            ParseFailed e -> do
                let (line, message) = locatedErrorMsg e
                dieWithLocation "<input>" line message
            ParseOk warnings x -> do
                mapM_ (warn normal . showPWarning "<input>") $ reverse warnings
                return x
    let pd = packageDescription gpd
        header = renderFields (renderPackageDesc pd)
        blocks = concat
            [ map renderSourceRepo (sourceRepos pd)
            , map renderFlags (genPackageFlags gpd)
            , maybeToList $ renderLibrary <$> condLibrary gpd
            , map (uncurry renderExe) (condExecutables gpd)
            , map (uncurry renderTest) (condTestSuites gpd)
            , maybeToList $ renderSetupBuildInfo <$> setupBuildInfo pd
            ]
        doc = vcat . intersperse L.empty
            $ header : map render blocks

    return $ displayS (renderPretty 1.0 width $ doc <> line) ""

renderSetupBuildInfo SetupBuildInfo{..} = Block
    (string "custom-setup")
    [mkField "setup-depends" setupDepends
        (not . null)
        (tokens . sort . map showDependency) ]

renderSourceRepo SourceRepo{..} = Block
    (string "source-repository" <+> showKind repoKind)
    [ maybeField "type" repoType (string . showType)
    , maybeField "location" repoLocation string
    , maybeField "subdir" repoSubdir fpToDoc
    , maybeField "tag" repoTag fpToDoc
    , maybeField "branch" repoBranch fpToDoc
    ]
    where
        showKind RepoHead = string "head"
        showKind RepoThis = string "this"
        showKind x        = error $ show x
        showType (OtherRepoType n) = n
        showType x                 = map toLower $ show x

renderPackageDesc pd@PackageDescription{..} =
    [ mkField' "name" (pkgName package) (string . unPackageName)
    , mkField' "version" (pkgVersion package) (string . showVersion)

    , mkNonempty "synopsis" synopsis string
    , longField "description" (normalizeDescription description) (const $ not $ null description)

    , mkField "license" license (/= UnspecifiedLicense) (string . showLicense)
    , ThingF $ license' licenseFiles

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

    , mkField' "cabal-version" (specVersion pd)
        (\ v -> string ">=" <> string (showVersion v))
    ] ++ map (\ (f,v) -> mkField' f v string) customFieldsPD
    where
        license' []  = noField
        license' [l] = Field "license-file" True (Left $ fpToDoc l)
        license' ls  = Field "license-files" True (Left $ tokens ls)
        replaceNl ('\n':'\n':xs) = '\n':'.':replaceNl ('\n':xs)
        replaceNl (x:xs)         = x:replaceNl xs
        replaceNl []             = []

normalizeDescription str = desc where
    chunks'' = splitOn "\n\n" str
    chunks' = map (strip . map (\ c -> if c == '\n' then ' ' else c)) chunks''
    strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse
    chunks = map (fillSep . map text . words) chunks'
    desc = vsep $ intersperse (string ".") chunks

renderTestedWith = fillSep . punctuate comma
    . map (\ (compiler, vers) ->
        string $ showDependency $ Dependency (mkPackageName (show compiler)) vers)

nonempty = nonempty' string
nonempty' f x y
    | null y = Nothing
    | otherwise = Just (x, f y)

showish = fish show

fish f n x = (,) n . string . f <$> x

-- plusConds components getBuildInfo
--     | null components = id
--     | otherwise = (<$$> vsep (empty : intersperse empty (map (renderComponent getBuildInfo) components)))

renderNodes f renderMore cs = concatMap (renderCondNode f renderMore) cs

renderExe exeName CondNode{..} = Block
    (string "executable" <+> string (unUnqualComponentName exeName))
    (renderExeData condTreeData ++ renderNodes buildInfo renderExeData condTreeComponents)

renderLibrary CondNode{..} = Block
    (string "library")
    (renderLibData condTreeData ++ renderNodes libBuildInfo renderLibData condTreeComponents)

renderTest testName CondNode{..} = Block
    (string "test-suite" <+> string (unUnqualComponentName testName))
    (renderTestData condTreeData ++ renderNodes testBuildInfo renderTestData condTreeComponents)

renderExeData Executable{..} =
    [ mkNonempty "main-is" modulePath string ]

renderLibData Library{..} =
    [ mkNonempty "exposed-modules" exposedModules modules' ]

renderTestData TestSuite{..} = case testInterface of
    TestSuiteExeV10 v f -> [ mkField' "type" "exitcode-stdio-1.0" string
                           , mkField' "main-is" f string
                           ]
    TestSuiteLibV09 v m -> [ mkField' "type" "detailed-0.9" string
                           , mkField' "test-module" m (string . intercalate "." . components)
                           ]
    TestSuiteUnsupported _ -> []

renderCondNode getBuildInfo extra (CondBranch pred branch1 branch2) =
    [ ThingB $ Block (string "if" <+> showPredicate pred)
        (extra (condTreeData branch1)
      ++ showBuildInfo (getBuildInfo $ condTreeData branch1)
      ++ renderNodes getBuildInfo extra (condTreeComponents branch1)) ]
    ++ maybeToList (flip fmap branch2
        $ \ b2 -> ThingB $ Block (string "else")
            (extra (condTreeData b2)
          ++ showBuildInfo (getBuildInfo $ condTreeData b2)
          ++ renderNodes getBuildInfo extra (condTreeComponents b2)))

showPredicate (Var x)    = showVar x
showPredicate (CNot p)   = string "!" <> showPredicate p
showPredicate (CAnd a b) = showPredicate a <+> string "&&" <+> showPredicate b
showPredicate (COr a b)  = showPredicate a <+> string "||" <+> showPredicate b
showPredicate (Lit b)    = string $ show b

showVar (Impl compiler vers) = string "impl" <> parens (string $
    (showDependency (Dependency (mkPackageName (map toLower $ show compiler)) vers)))
showVar (Flag f) = string "flag" <> parens (string (unFlagName f))
showVar (OS w) = string "os" <> parens (string $ map toLower $ show w)
showVar (Arch a) = string "arch" <> parens (string $ map toLower $ show a)

showBuildInfo BuildInfo{..} = if any (\ (ThingF f) -> fFilter f) defaults
    then defaults
    -- hack: rarely if ever do we want to render an empty build-info
    -- the "buildable" attribute is always present and nearly always True,
    -- but that would result in it showing up in every condtree
    --
    -- if build-info is empty, show buildable: True
    else [ mkField' "buildable" buildable (string . show) ]
    where
        defaults =
            [ mkNonempty "hs-source-dirs" hsSourceDirs files
            , maybeField "default-language" defaultLanguage (string . show)
            , mkNonempty "default-extensions" defaultExtensions (tokens . sort . map showExtension . sort)
            , mkNonempty "build-depends" targetBuildDepends (tokens . map showDependency . sort)
            , mkNonempty "extensions" oldExtensions (tokens . sort . map showExtension . sort)
            , mkNonempty "cpp-options" cppOptions tokens'
            , mkNonempty "other-modules" otherModules modules'
            , mkNonempty "extra-libraries" extraLibs tokens
            , mkNonempty "frameworks" frameworks tokens
            , mkNonempty "other-extensions" otherExtensions (modules showExtension)
            , mkField "buildable" (string "False") (\ _ -> not buildable) id
            ] ++ map renderOption options ++
            [ mkNonempty "c-sources" cSources files
            , mkNonempty "include-dirs" includeDirs files
            ]

renderOption (f, args) = mkField'
    (map toLower (show f) ++ "-options")
    args
    tokens'

modules' = modules (intercalate "." . components)

modules f mnames = align $ vcat $ map string $ sort $ map f mnames

-- showOptions

tokens = fillSep . punctuate comma . map string
tokens' = fillSep . map string

files = fillSep . punctuate comma . map fpToDoc

fpToDoc x
    | null x = string "\"\""
    | any isSpace x = string $ show x
    | otherwise = string x

values vs = vcat $ map (\ (x,y) -> showPair x y) $ vs
    where
        (xs, ys) = unzip vs
        width = foldr max 0 (map length xs) + 2
        showPair "description" desc = string "description:" L.<$> indent 2 desc
        showPair x y = fill width (string x <> colon) <> align y

showDependency (Dependency pn v)
    | v == anyVersion = unPackageName pn
    | otherwise = unPackageName pn ++ " " ++
        foldVersionRange' ""
            (\ v -> "== " ++ showVersion v)
            (\ v -> "> " ++ showVersion v)
            (\ v -> "< " ++ showVersion v)
            (\ v -> ">= " ++ showVersion v)
            (\ v -> "<= " ++ showVersion v)
            (\ v _ -> "== " ++ showVersion v ++ ".*")
            (\ v _ -> "^>= " ++ showVersion v)
            (\ a b -> a ++ " || " ++ b)
            (\ a b -> a ++ " && " ++ b)
            (\ a -> "(" ++ a ++ ")")
            v

showExtension (EnableExtension s)  = show s
showExtension (DisableExtension s) = "No" ++ show s

instance Ord Dependency where
    compare (Dependency p _) _ | unPackageName p == "base" = LT
    compare _ (Dependency p _) | unPackageName p == "base" = GT
    compare (Dependency d1 _) (Dependency d2 _)   = compare d1 d2

renderFlags MkFlag{..} = Block
    (string "flag" <+> string fname)
    [ mkField' "default" flagDefault (string . show)
    , mkField "manual" (string "True") (const flagManual) id
    , longField "description" (normalizeDescription flagDescription) (const $ not $ null flagDescription)
    ]
    where fname = unFlagName flagName

showLicense MIT          = "MIT"
showLicense BSD2         = "BSD2"
showLicense BSD3         = "BSD3"
showLicense BSD4         = "BSD4"
showLicense PublicDomain = "PublicDomain"
showLicense ISC          = "ISC"
showLicense (MPL v)      = showL "MPL" (Just v)
showLicense (LGPL v)     = showL "LGPL" v
showLicense (GPL v)      = showL "GPL" v
showLicense (AGPL v)     = showL "AGPL" v
showLicense (Apache v)   = showL "Apache" v
showLicense OtherLicense = "OtherLicense"
showLicense x            = error $ show x

showL s Nothing  = s
showL s (Just v) = s ++ "-" ++ showVersion v
