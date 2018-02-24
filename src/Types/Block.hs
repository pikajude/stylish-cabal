module Types.Block where

import Distribution.PackageDescription
import Prelude.Compat
import Types.Field

type File = ([Maybe Field], [Block])

data Block = Block
    { title :: BlockHead
    , fields :: [Maybe Field]
    , subBlocks :: [Block]
    } deriving (Show)

data BlockHead
    = If (Condition ConfVar)
    | Else
    | Benchmark_ String
    | TestSuite_ String
    | Exe_ String
    | Library_ (Maybe String)
    | ForeignLib_ String
    | Flag_ String
    | SourceRepo_ RepoKind
    | CustomSetup
    deriving (Show)

isElse Else = True
isElse _ = False
