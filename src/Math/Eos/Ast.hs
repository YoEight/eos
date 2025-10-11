module Math.Eos.Ast where

import Math.Eos.Mu
import Math.Eos.Sym

data Sign = SignNeg | SignPos deriving (Show, Eq)

type Ast = Mu Term

data Term a
    = TermNumber Sign Int
    | TermVar String Int
    | TermBinary Operator a a

instance Functor Term where
    fmap _ (TermNumber s n) = TermNumber s n
    fmap _ (TermVar n c) = TermVar n c
    fmap f (TermBinary op lhs rhs) = TermBinary op (f lhs) (f rhs)
