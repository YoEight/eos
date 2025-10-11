module Math.Eos.Sym where

import Data.Int
import Data.Text

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
    | SymVar !Text
    | SymOp !Operator
    | SymLParens
    | SymRParens
    deriving (Eq, Show)
