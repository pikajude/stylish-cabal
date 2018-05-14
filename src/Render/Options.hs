{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Render.Options
  ( module Render.Options
  , module Control.Monad.Reader
  ) where

import Control.Monad.Reader hiding (mapM)
import Data.Data
import Data.Default
import GHC.Generics
import Prelude.Compat
import Text.PrettyPrint.ANSI.Leijen (Doc, indent, width)

type Render = Reader RenderOptions

data RenderOptions = RenderOptions
  { -- | Number of spaces to use for indentation.
  indentSize :: Int
  -- | If 'True', @stylish-cabal@ will use
  -- 'Distribution.Version.simplifyVersionRange' to simplify every
  -- version range present in the Cabal file. For example, it'll turn
  -- a constraint like @>= 3.0 && <= 3.0@ into just @== 3.0@.
  , simplifyVersions :: Bool
  } deriving (Show, Eq, Generic, Typeable, Data)

instance Default RenderOptions where
  def = RenderOptions {indentSize = 2, simplifyVersions = False}

indentM :: Doc -> Render Doc
indentM k = do
  s <- asks indentSize
  return $ indent s k

(<&>) = flip fmap

widthR :: Doc -> (Int -> Render Doc) -> Render Doc
widthR = liftRender . width

liftRender :: ((a -> b) -> c) -> (a -> Render b) -> Render c
liftRender f g = ask <&> \env -> f $ \fn -> runReader (g fn) env
