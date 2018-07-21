module StylishCabal where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.ByteString as B
import Data.Foldable
import Data.Functor.Identity
import Distribution.CabalSpecVersion
import Parse
import Pretty
import Stylish.Monad
import System.Exit
import System.IO
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

prettyPrintFile f = do
    b <- B.readFile f
    r <-
        runExceptT $ do
            (sv, parsed) <- withExceptT MiscError $ parseCabalFile b
            evalStylish (pprFields parsed) sv
    return $ renderPretty 1.0 90 <$> r

doFile f = traverse_ (displayIO stdout) =<< prettyPrintFile f
