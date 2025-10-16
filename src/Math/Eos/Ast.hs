module Math.Eos.Ast where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Data.Functor.Classes

import Math.Eos.Mu
import Math.Eos.Parser
import Math.Eos.Sym

type Ast = Mu Term

data Term a
    = TermNumber Int
    | TermVar String Int
    | TermBinary Operator a a
    | TermUnary Operator a
    | TermGroup a
    deriving (Eq)

instance Eq1 Term where
    liftEq _ (TermNumber x) (TermNumber y) = x == y
    liftEq _ (TermVar x n) (TermVar y m) = x == y && n == m
    liftEq f (TermBinary opa la ra) (TermBinary opb lb rb) = opa == opb && f la lb && f ra rb
    liftEq f (TermUnary opa a) (TermUnary opb b) = opa == opb && f a b
    liftEq f (TermGroup a) (TermGroup b) = f a b
    liftEq _ _ _ = False

instance Functor Term where
    fmap _ (TermNumber n) = TermNumber n
    fmap _ (TermVar n c) = TermVar n c
    fmap f (TermBinary op lhs rhs) = TermBinary op (f lhs) (f rhs)

number :: Int -> Ast
number = Mu . TermNumber

var :: String -> Ast
var n = Mu $ TermVar n 1

binary :: Operator -> Ast -> Ast -> Ast
binary op lhs rhs = Mu $ TermBinary op lhs rhs

unary :: Operator -> Ast -> Ast
unary op = Mu . TermUnary op

group :: Ast -> Ast
group = Mu . TermGroup

parseAst :: Parser Sym Ast
parseAst = go =<< lookAheadOrBail
  where
    go (SymNumber _) = parseBinary 0
    go (SymVar _) = parseBinary 0
    go SymLParens = parseGroup
    go (SymOp Sub) = parseUnary
    go x = throwError $ "unexpected symbol '" ++ show x ++ "'"

    parseUnary = do
        shift
        unary Sub <$> parseAst

parseBinary :: Int -> Parser Sym Ast
parseBinary min_bind = do
    sym <- lookAheadOrBail
    lhs <- if sym == SymLParens then parseGroup else parsePrimary
    loop lhs
  where
    loop lhs = do
        opt <- lookAhead
        case opt of
            Nothing -> pure lhs
            Just sym
                | sym == SymRParens -> pure lhs
                | otherwise ->
                    case sym of
                        SymOp op ->
                            let (lhs_bind, rhs_bind) = bindingPow op
                             in if lhs_bind < min_bind
                                    then pure lhs
                                    else loop . binary op lhs =<< parseBinary rhs_bind
                        x -> throwError $ "expected an operator but got '" ++ show x ++ "'"

parsePrimary :: Parser Sym Ast
parsePrimary = go =<< shift
  where
    go (SymNumber n) = pure $ number n
    go (SymVar n) = pure $ var n
    go x = throwError $ "expected a number or a variable but got '" ++ show x ++ "' instead"

parseGroup :: Parser Sym Ast
parseGroup = do
    open <- shift

    when (open /= SymLParens) $
        throwError $
            "expected '(' but got '" ++ show open ++ "'"

    ast <- parseAst
    close <- shift

    when (close /= SymLParens) $
        throwError $
            "expected '(' but got '" ++ show close ++ "'"

    pure $ group ast

bindingPow :: Operator -> (Int, Int)
bindingPow Add = (10, 10)
bindingPow Sub = (10, 10)
bindingPow Mul = (20, 20)
bindingPow Div = (20, 20)
bindingPow Exp = (30, 29)
bindingPow Equal = (1, 1)
