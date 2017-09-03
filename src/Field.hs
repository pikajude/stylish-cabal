{-# Language FlexibleContexts  #-}
{-# Language FlexibleInstances #-}
{-# Language RecordWildCards   #-}

module Field where

import Control.Monad
import Control.Monad.Reader
import Data.IORef
import Data.Maybe
import Debug.Trace
import Prelude                 hiding ((<$>))
import System.IO.Unsafe
import Text.PrettyPrint.Leijen

type StyleM = ReaderT FormatOpts IO

data FormatOpts = FormatOpts
                { pageWidth  :: Int
                , indentSize :: Int
                } deriving Show

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

mkField n v f d = return $ ThingF $ Field n (f v) (Left $ d v)

mkField' n v = mkField n v (const True)

noField = Field "foo" False (Left empty)

mkNonempty n v = mkField n v (not . null)

mkBlock :: Monad m => Doc -> [m Thing] -> m Block
mkBlock n fs = Block n `fmap` sequence fs

longField n v f = do
    pw <- asks pageWidth
    return $ ThingF $ Field n (f v) $ Right
        $ \ width key -> if length (show v) + width <= pw
            then fill width key <> align v
            else fillBreak 2 key <> align v

maybeField n v d = mkField n v isJust (d . fromJust)

render Block{..} = do
    indent' <- asks indentSize
    fs <- renderFields blkFields
    return $ blkName <$$> indent indent' fs

renderFields blkFields = do
    docs <- mapM (renderField width) blkFields
    return $ vcat $ catMaybes docs
    where
        width = foldr max 0 (mapMaybe (fmap length . getFName) blkFields) + 2
        getFName (ThingF f)
            | fFilter f = Just $ fName f
        getFName _ = Nothing

renderField :: Int -> Thing -> StyleM (Maybe Doc)
renderField _ (ThingF Field { fFilter = False }) = return Nothing
renderField w (ThingF Field{..}) = pure $ Just $ case fRender of
    Left simple -> fill w (string fName <> colon) <> align simple
    Right f     -> f w (string fName <> colon)
renderField _ (ThingB b)
    | show (blkName b) == "else" = Just `fmap` render b
    | otherwise = do
        doc <- render b
        return $ Just $ empty <$> doc
