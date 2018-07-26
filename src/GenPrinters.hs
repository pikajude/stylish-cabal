module GenPrinters where

import Control.Monad
import Data.Proxy (Proxy(..))
import Language.Haskell.TH

printers :: [(String, TypeQ)] -> (ExpQ -> ExpQ) -> Q [Dec]
printers s fallback = do
    name <- newName "name"
    [d| docField $( varP name ) = $( caseE (varE name) $ mkCases name ++ [otherCase] ) |]
  where
    mkCases n =
        flip map s $ \(str, ty) ->
            match
                (litP $ stringL str)
                (normalB [|fieldPrinter (Proxy :: Proxy $(ty)) $(stringE str)|])
                []
    otherCase =
        newName "e" >>= \e -> match (varP e) (normalB (fallback (varE e))) []
