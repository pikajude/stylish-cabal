{-# LANGUAGE FlexibleContexts #-}

module Stylish.Monad where

import Control.Exception
import Control.Monad.Except
import Control.Monad.RWS
import qualified Data.ByteString as B
import Distribution.CabalSpecVersion
import qualified Text.Parsec as P

type SectionName = B.ByteString

type StylishT m = RWST CabalSpecVersion () (Maybe SectionName) (ExceptT StylishErr m)

data StylishErr
    = ParseError P.ParseError
    | MiscError String
    deriving (Show)

instance Exception StylishErr

evalStylish b r = fst <$> evalRWST b r Nothing

setSectionName r = do
    b <- get
    case b of
        Nothing -> put (Just r)
        Just {} -> return ()

clearSectionName = put Nothing
