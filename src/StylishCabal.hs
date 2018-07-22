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
    prettyPrintBytes b

prettyPrintBytes b = do
    r <-
        runExceptT $ do
            (sv, parsed) <- parseCabalFile b
            evalStylish (pprFields parsed) sv
    return $ renderPretty 1.0 90 <$> r

doFile f = do
    x <- prettyPrintFile f
    case x of
        Right m -> displayIO stdout m
        Left y -> print y
