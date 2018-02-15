module Parse
    ( parse
    , displayError
    , printWarnings
    , Result(..)
    ) where

import Distribution.PackageDescription.Parse
import Distribution.ParseUtils
import Distribution.Simple.Utils
import Distribution.Verbosity
import System.Exit

data Result a
    = Error (Maybe LineNo)
            String
    | Warn [PWarning]
    | Success a

instance Functor Result where
    fmap f (Success a) = Success (f a)
    fmap _ (Warn ps) = Warn ps
    fmap _ (Error m s) = Error m s

parse input =
    case parseGenericPackageDescription input of
        ParseFailed e -> uncurry Error $ locatedErrorMsg e
        ParseOk warnings x
            | null warnings -> Success x
            | otherwise -> Warn $ reverse warnings

displayError line' message = dieWithLocation' normal "<input>" line' message

printWarnings ps = mapM_ (warn normal . showPWarning "<input>") ps >> exitFailure
