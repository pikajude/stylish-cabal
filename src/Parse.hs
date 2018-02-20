{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language CPP #-}
{-# Language StandaloneDeriving #-}
{-# Language DeriveGeneric #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveFunctor #-}

module Parse
    ( parseCabalFile
    , readCabalFile
    , displayError
    , printWarnings
    , Result(..)
    , result
    ) where

import Control.DeepSeq
import Data.Data
import Data.Maybe
import Distribution.PackageDescription.Parse
import Distribution.ParseUtils
import Distribution.Simple.Utils
import Distribution.Verbosity
import GHC.Generics
import System.Environment
import System.Exit
import System.IO

-- | Like Cabal's @ParseResult@, but treats warnings as a separate failure
-- case.
data Result a
    = Error (Maybe LineNo)
            String -- ^ Parse error on the given line.
    | Warn [PWarning] -- ^ Warnings emitted during parse.
    | Success a -- ^ The input is a compliant package description.
    deriving (Show, Eq, Functor, Generic, Typeable, Data)

-- | Case analysis for 'Result'.
result :: (Maybe LineNo -> String -> b) -> ([PWarning] -> b) -> (a -> b) -> Result a -> b
result e w s p =
    case p of
        Error l m -> e l m
        Warn ws -> w ws
        Success r -> s r

instance NFData a => NFData (Result a)

deriving instance Generic PWarning

deriving instance Data PWarning

#if !MIN_VERSION_base(4,8,0)
deriving instance Typeable PWarning
#endif

instance NFData PWarning

-- | This function is similar to Cabal's own file parser, except that it
-- treats warnings as a separate failure case. There are a wide range of
-- different behaviors accepted by different Cabal parser versions. Parse
-- warnings generally indicate a version-related inconsistency, so we play
-- it safe here.
parseCabalFile input =
    case parseGenericPackageDescription input of
        ParseFailed e -> uncurry Error $ locatedErrorMsg e
        ParseOk warnings x
            | null warnings -> Success x
            | otherwise -> Warn $ reverse warnings

-- | Shorthand to combine 'parseCabalFile' and one of 'printWarnings' or
-- 'displayError'. The given 'FilePath' is used only for error messages and
-- is not read from.
readCabalFile fpath = result (displayError fpath) printWarnings return . parseCabalFile

-- | Print some warnings to 'stderr' and exit.
printWarnings ps = mapM_ (warn normal . showPWarning "<input>") ps >> exitFailure

-- | Print a parse error to 'stderr', annotated with filepath and line
-- number (if available), then exit.
displayError :: Maybe FilePath -> Maybe LineNo -> String -> IO a
displayError fpath line' message = do
    prog <- getProgName
    hPutStrLn stderr $
        prog ++
        ": " ++
        fromMaybe "<input>" fpath ++
        (case line' of
             Just lineno -> ":" ++ show lineno
             Nothing -> "") ++
        ": " ++ message
    exitFailure
