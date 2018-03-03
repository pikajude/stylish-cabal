{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language CPP #-}
{-# Language StandaloneDeriving #-}
{-# Language DeriveGeneric #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveFunctor #-}

module Parse
    ( parsePackageDescription
    , readPackageDescription
    , displayError
    , printWarnings
    , Result(..)
    , result
    ) where

import Control.DeepSeq
import Control.Monad.Compat
import Data.Data
import Data.Maybe
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
    ( parseGenericPackageDescription
    , runParseResult
    )
import Distribution.Parsec.Common
import Distribution.Simple.Utils
import Distribution.Verbosity
import Distribution.Version
import GHC.Generics
import Prelude.Compat
import System.Exit

#if MIN_VERSION_base(4,9,0)
import Data.Functor.Classes
#endif

-- | Like Cabal's @ParseResult@, but treats warnings as a separate failure
-- case.
data Result a
    = Error [PError] -- ^ Parse errors.
    | Warn [PWarning] -- ^ Warnings emitted during parse.
    | Success a -- ^ The input is a compliant package description.
    deriving (Show, Eq, Functor, Generic, Typeable, Data)

#if MIN_VERSION_base(4,6,0)
deriving instance Generic1 Result
#endif

#if MIN_VERSION_base(4,9,0)
instance Show1 Result where
    liftShowsPrec sp _ p (Success n) = showsUnaryWith sp "Success" p n
    liftShowsPrec _ _ p (Warn pws) = showsUnaryWith showsPrec "Warn" p pws
    liftShowsPrec _ _ p (Error s) = showsUnaryWith showsPrec "Error" p s

instance Eq1 Result where
    liftEq eq (Success a) (Success b) = eq a b
    liftEq _ (Error s) (Error s2) = s == s2
    liftEq _ (Warn pws) (Warn pws2) = pws == pws2
    liftEq _ _ _ = False
#endif

-- | Case analysis for 'Result'.
result :: ([PError] -> b) -> ([PWarning] -> b) -> (a -> b) -> Result a -> b
result e w s p =
    case p of
        Error l -> e l
        Warn ws -> w ws
        Success r -> s r

deriving instance Data PError

deriving instance Eq PError

deriving instance Eq PWarning

instance NFData a => NFData (Result a)

deriving instance Generic PError

instance NFData Position

deriving instance Generic Position

instance NFData PWarning

deriving instance Generic PWarning

instance NFData PWarnType

deriving instance Generic PWarnType

instance NFData PError

deriving instance Data PWarning

deriving instance Data Position

deriving instance Data PWarnType

deriving instance Typeable PWarning

-- | This function is similar to Cabal's own file parser, except that it
-- treats warnings as a separate failure case. There are a wide range of
-- different behaviors accepted by different Cabal parser versions. Parse
-- warnings generally indicate a version-related inconsistency, so we play
-- it safe here.
parsePackageDescription input =
    let (warnings, r) = runParseResult $ parseGenericPackageDescription input
     in case r of
            Left (_, errors) -> Error errors
            Right x
                | null warnings -> parseResult x
                | otherwise -> Warn warnings
  where
    parseResult gpd =
        if specVersionRaw (packageDescription gpd) == Right anyVersion
            then Warn [PWarning PWTOther zeroPos versWarning]
            else Success gpd
    versWarning =
        "File does not specify a cabal-version. stylish-cabal requires at least 1.2"

-- | Shorthand to combine 'parsePackageDescription' and one of 'printWarnings' or
-- 'displayError'. The given 'FilePath' is used only for error messages and
-- is not read from.
readPackageDescription fpath =
    result (displayError fpath) (printWarnings fpath) return . parsePackageDescription

-- | Print some warnings to 'stderr' and exit.
printWarnings :: Maybe FilePath -> [PWarning] -> IO a
printWarnings fpath ps =
    mapM_ (warn normal . showPWarning (fromMaybe "<input>" fpath)) ps >> exitFailure

-- | Print a parse error to 'stderr', annotated with filepath if available,
-- then exit.
displayError :: Maybe FilePath -> [PError] -> IO a
displayError fpath warns =
    mapM_ (warn normal . showPError (fromMaybe "<input>" fpath)) warns >> exitFailure
