module StylishCabal where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.ByteString as B
import Data.Functor.Identity
import Distribution.CabalSpecVersion
import Parse
import Pretty
import System.Exit
import System.IO
import Text.PrettyPrint.ANSI.Leijen

prettyPrintFile f = do
    b <- B.readFile f
    parsed' <- runExceptT (parseCabalFile b)
    let parsed = either (error . show) id parsed'
    either
        (die . show)
        (displayIO stdout . renderPretty 1.0 90)
        (runIdentity $ runExceptT $ runReaderT (pprFields parsed) CabalSpecV1_24)
