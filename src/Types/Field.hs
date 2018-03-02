{-# Language NoMonomorphismRestriction #-}

module Types.Field
    ( Field(..)
    , FieldVal(..)
    , extensions
    , rexpModules
    , flibOptions
    , file
    , flibType
    , fieldName
    , spaces
    , commas
    , toolDepends
    , oldToolDepends
    , pcDepends
    , buildDeps
    , modules
    , module_
    , mixins_
    , desc
    , version
    , cabalVersion
    , longList
    , testedField
    , licenseField
    , dependencies
    , nonEmpty
    , stringField
    , ffilter
    ) where

import Distribution.Compiler
import Distribution.License
import Distribution.ModuleName
import Distribution.Types.Dependency
import Distribution.Types.ExeDependency
import Distribution.Types.ForeignLibOption
import Distribution.Types.ForeignLibType
import Distribution.Types.LegacyExeDependency
import Distribution.Types.Mixin
import Distribution.Types.ModuleReexport
import Distribution.Types.PkgconfigDependency
import Distribution.Version
import Documentation.Haddock.Parser
import Documentation.Haddock.Types (_doc, DocH)
import Language.Haskell.Extension
import Prelude.Compat

data FieldVal
    = Dependencies [Dependency]
    | Version Version
    | CabalVersion (Either Version VersionRange)
    | License License
    | Str String
    | File String
    | Spaces [String]
    | Commas [String]
    | LongList [String]
    | Extensions [Extension]
    | Modules [ModuleName]
    | Module ModuleName
    | RexpModules [ModuleReexport]
    | TestedWith [(CompilerFlavor, VersionRange)]
    | ToolDepends [ExeDependency]
    | OldToolDepends [LegacyExeDependency]
    | PcDepends [PkgconfigDependency]
    | Mixins [Mixin]
    | FlibType ForeignLibType
    | FlibOptions [ForeignLibOption]
    deriving (Show)

data Field
    = Description (DocH () Identifier)
    | Field String
            FieldVal
    deriving (Show)

flibType n p = Just $ Field n (FlibType p)

flibOptions n p = Just $ Field n (FlibOptions p)

toolDepends n as = Just $ Field n (ToolDepends as)

oldToolDepends n as = Just $ Field n (OldToolDepends as)

pcDepends n as = Just $ Field n (PcDepends as)

rexpModules n as = Just $ Field n (RexpModules as)

mixins_ n as = Just $ Field n (Mixins as)

dependencies n as = Just $ Field n (Dependencies as)

licenseField n a = Just $ Field n (License a)

file n a = Just $ Field n (File a)

spaces n as = Just $ Field n (Spaces as)

commas n as = Just $ Field n (Commas as)

longList n as = Just $ Field n (LongList as)

extensions n as = Just $ Field n (Extensions as)

version n a = Just $ Field n (Version a)

cabalVersion _ (Right x)
    | x == anyVersion = Nothing
cabalVersion n a = Just $ Field n (CabalVersion a)

modules n as = Just $ Field n (Modules as)

module_ n a = Just $ Field n (Module a)

testedField n a = Just $ Field n (TestedWith a)

ffilter f g as
    | not (f as) = Nothing
    | otherwise = g as

nonEmpty = ffilter (not . null)

fieldName (Field s _) = s
fieldName (Description _) = "description"

desc [] = Nothing
desc vs = Just (Description $ _doc $ parseParas vs)

buildDeps [] = Nothing
buildDeps vs = dependencies "build-depends" vs

stringField n p = Just (Field n (Str p))
