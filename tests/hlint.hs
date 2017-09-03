import Control.Monad
import Language.Haskell.HLint
import System.Exit

main :: IO ()
main = hlint ["."] >>= \ hints -> unless (null hints) exitFailure
