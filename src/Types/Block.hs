module Types.Block where

import Distribution.PackageDescription
import Types.Field

type File c = ([Maybe Field], [Block c])

data Block c = Block
    { title :: BlockHead c
    , fields :: [Maybe Field]
    , subBlocks :: [Block c]
    }
    deriving Show

data BlockHead c = If (Condition c)
                 | Else
                 | Benchmark_ String
                 | TestSuite_ String
                 | Exe_ String
                 | Library_
                 | Flag_ String
                 | SourceRepo_ RepoKind
                 | CustomSetup
                 deriving Show

isElse Else = True
isElse _ = False
