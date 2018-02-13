module Parse (parse) where

import Distribution.PackageDescription.Parse
import Distribution.ParseUtils
import Distribution.Simple.Utils
import Distribution.Verbosity

parse input = do
    let res = parseGenericPackageDescription input
    case res of
        ParseFailed e -> do
            let (line', message) = locatedErrorMsg e
            dieWithLocation' normal "<input>" line' message
        ParseOk warnings x -> do
            mapM_ (warn normal . showPWarning "<input>") $ reverse warnings
            return x
