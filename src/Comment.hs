module Comment where

import qualified Data.ByteString as B
import Data.List.NonEmpty (NonEmpty)

data CommentOr a
    = MkComment B.ByteString
    | NotComment (NonEmpty a)
    deriving (Show, Eq)
