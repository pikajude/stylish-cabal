module Parse where

import Distribution.PackageDescription.Parse

parse input = do
    let res = parseGenericPackageDescription input
    case res of
        ParseFailed e -> do
            let (line', message) = locatedErrorMsg e
            liftIO $ dieWithLocation' normal "<input>" line' message
        ParseOk warnings x -> do
            mapM_ (liftIO . warn normal . showPWarning "<input>") $ reverse warnings
            return x
