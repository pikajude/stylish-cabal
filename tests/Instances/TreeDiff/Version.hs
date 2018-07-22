module Instances.TreeDiff.Version where

import Data.TreeDiff
import Distribution.Version (Version, VersionRange)

instance ToExpr Version where
    toExpr = defaultExprViaShow

instance ToExpr VersionRange
