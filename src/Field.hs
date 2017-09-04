{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language FlexibleContexts  #-}
{-# Language FlexibleInstances #-}
{-# Language RecordWildCards   #-}

module Field (
    FormatOpts(..), Thing(..), Field(..), Block(..), StyleM,
    longField, maybeField, mkBlock, mkField, mkField', mkNonempty, noField,
    render, renderFields,
) where

import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import Prelude                 hiding ((<$>))
import Text.PrettyPrint.Leijen

type StyleM = ReaderT FormatOpts IO

data FormatOpts = FormatOpts
                { pageWidth  :: Int
                , indentSize :: Int
                } deriving Show

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

mkField :: Monad m => String -> t -> (t -> Bool) -> (t -> Doc) -> m Thing
mkField n v f d = return $ ThingF $ Field n (f v) (Left $ d v)

mkField' :: Monad m => String -> b -> (b -> Doc) -> m Thing
mkField' n v = mkField n v (const True)

noField :: Field
noField = Field "foo" False (Left empty)

mkNonempty :: (Foldable t, Monad m)
           => String -> t a -> (t a -> Doc) -> m Thing
mkNonempty n v = mkField n v (not . null)

mkBlock :: Monad m => Doc -> [m Thing] -> m Block
mkBlock n fs = Block n `fmap` sequence fs

longField :: MonadReader FormatOpts m => String -> Doc -> (Doc -> Bool) -> m Thing
longField n v f = do
    pw <- asks pageWidth
    return $ ThingF $ Field n (f v) $ Right
        $ \ width' key -> if length (show v) + width' <= pw
            then fill width' key <> align v
            else fillBreak 2 key <> align v

maybeField :: Monad m => String -> Maybe b -> (b -> Doc) -> m Thing
maybeField n v d = mkField n v isJust (d . fromJust)

render :: Block -> StyleM Doc
render Block{..} = do
    indent' <- asks indentSize
    fs <- renderFields blkFields
    return $ blkName <$$> indent indent' fs

renderFields :: [Thing] -> StyleM Doc
renderFields blkFields = do
    docs <- mapM (renderField width') blkFields
    return $ vcat $ catMaybes docs
    where
        width' = foldr max 0 (mapMaybe (fmap length . getFName) blkFields) + 2
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
