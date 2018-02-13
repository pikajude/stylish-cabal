{-# Language NoMonomorphismRestriction #-}

module Types.Field
    ( Field(..)
    , FieldVal(..)
    , extensions
    , file
    , fieldName
    , spaces
    , commas
    , buildDeps
    , modules
    , module_
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
import Distribution.Version
import Language.Haskell.Extension

data FieldVal
    = Dependencies [Dependency]
    | Version Version
    | CabalVersion Version
    | License License
    | Str String
    | File String
    | Spaces [String]
    | Commas [String]
    | LongList [String]
    | Extensions [Extension]
    | Modules [ModuleName]
    | Module ModuleName
    | TestedWith [(CompilerFlavor, VersionRange)]
    deriving (Show)

data Field
    = Description String
    | Field String
            FieldVal
    deriving (Show)

dependencies n as = Just $ Field n (Dependencies as)

licenseField n a = Just $ Field n (License a)

file n a = Just $ Field n (File a)

spaces n as = Just $ Field n (Spaces as)

commas n as = Just $ Field n (Commas as)

longList n as = Just $ Field n (LongList as)

extensions n as = Just $ Field n (Extensions as)

version n a = Just $ Field n (Version a)

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
desc vs = Just (Description vs)

buildDeps [] = Nothing
buildDeps vs = dependencies "build-depends" vs

stringField n p = Just (Field n (Str p))
