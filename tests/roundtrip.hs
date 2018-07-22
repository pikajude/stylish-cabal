import Control.Monad
import qualified Data.ByteString as B
import System.Directory
import System.FilePath
import Test.Hspec
import TestCommon

mkTest path = (,) path <$> B.readFile path

main = do
    textfiles <- listDirectory "tests/cabal-files"
    tests <- mapM (\x -> mkTest ("tests/cabal-files" </> x)) textfiles
    hspec $ describe "stylish-cabal" $ forM_ tests testFile
