module Math.Eos.Sym where

import Data.Int

data Operator
    = Add
    | Sub
    | Mul
    | Div
    | Exp
    | Equal
    deriving (Eq, Show)

data Sym
    = SymNumber !Int
    | SymVar !String
    | SymOp !Operator
    | SymLParens
    | SymRParens
    deriving (Eq, Show)

data Pos = Pos {posLine :: !Int, posCol :: !Int} deriving (Eq)

instance Show Pos where
    show p = show (posLine p) ++ ":" ++ show (posCol p)

posDefault :: Pos
posDefault = Pos{posLine = 1, posCol = 1}

posColIncr :: Pos -> Pos
posColIncr p = p{posCol = posCol p + 1}
