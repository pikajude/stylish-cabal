{-# LANGUAGE FlexibleContexts #-}

module Stylish.Monad where

import Control.Exception
import Control.Monad.Except
import Control.Monad.RWS
import qualified Data.ByteString as B
import Distribution.CabalSpecVersion
import Distribution.Parsec.FieldLineStream
import qualified Text.Parsec as P

type SectionName = B.ByteString

type StylishT m = RWST CabalSpecVersion () [SectionName] (ExceptT StylishErr m)

data StylishErr
    = ParseError (Maybe FieldLineStream, P.ParseError)
    | MiscError String
    deriving (Show)

instance Exception StylishErr

evalStylish b r = fst <$> evalRWST b r []

pushSectionName r = modify (++ [r])

popSectionName = modify init
