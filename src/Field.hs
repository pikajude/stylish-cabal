{-# Language FlexibleInstances #-}
{-# Language RecordWildCards   #-}

module Field where

import Control.Monad
import Data.IORef
import Data.Maybe
import Debug.Trace
import Prelude                 hiding ((<$>))
import System.IO.Unsafe
import Text.PrettyPrint.Leijen

wideness :: IORef Int
wideness = unsafePerformIO $ newIORef 0
{-# NOINLINE wideness #-}

data Field = Field
           { fName   :: String
           , fFilter :: Bool
           , fRender :: Either Doc (Int -> Doc -> Doc)
           } deriving Show

instance Show (Int -> Doc -> Doc) where show _ = "<fn>"

data Block = Block
           { blkName   :: Doc
           , blkFields :: [Thing]
           } deriving Show

data Thing = ThingF Field | ThingB Block
           deriving Show

mkField n v f d = ThingF $ Field n (f v) (Left $ d v)

mkField' n v = mkField n v (const True)

noField = Field "foo" False (Left empty)

mkNonempty n v = mkField n v (not . null)

longField n v f = ThingF $ Field n (f v) $ Right
    $ \ width key -> if length (show v) + width <= unsafePerformIO (readIORef wideness)
        then fill width key <> align v
        else fillBreak 2 key <> align v

maybeField n v d = mkField n v isJust (d . fromJust)

render Block{..} = blkName <$$> indent 2 (renderFields blkFields)

renderFields blkFields = vcat $ mapMaybe (renderField width) blkFields
    where
        width = foldr max 0 (mapMaybe (fmap length . getFName) blkFields) + 2
        getFName (ThingF f)
            | fFilter f = Just $ fName f
        getFName _ = Nothing

renderField _ (ThingF (Field { fFilter = False })) = Nothing
renderField w (ThingF Field{..}) = Just $ case fRender of
    Left simple -> fill w (string fName <> colon) <> align simple
    Right f     -> f w (string fName <> colon)
renderField _ (ThingB b)
    | show (blkName b) == "else" = Just $ render b
    | otherwise = Just $ empty <$> render b
