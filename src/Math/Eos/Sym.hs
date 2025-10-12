module Math.Eos.Sym where

import Data.Int

data Operator
    = Add
    | Sub
    | Mul
    | Div
    | Exp
    | Equal
    deriving (Eq)

instance Show Operator where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Exp = "^"
    show Equal = "="

data Sym
    = SymNumber !Int
    | SymVar !String
    | SymOp !Operator
    | SymLParens
    | SymRParens
    deriving (Eq)

instance Show Sym where
    show (SymNumber n) = show n
    show (SymVar n) = n
    show (SymOp op) = show op
    show SymLParens = "("
    show SymRParens = ")"
