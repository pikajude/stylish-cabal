import Control.Monad
import Language.Haskell.HLint
import System.Exit

main :: IO ()
main = hlint [".", "--cpp-define=TEST_HACKAGE=1"] >>= \ hints -> unless (null hints) exitFailure
