module Math.Eos.Ast where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Data.Fix (Fix (..))
import Data.Foldable
import Data.Functor.Classes

import Math.Eos.Parser
import Math.Eos.Sym

type Ast = Fix Ast'

data Ast' a
    = AstNumber Int
    | AstVar String Int
    | AstBinary Operator a a
    | AstUnary Operator a
    | AstGroup a
    deriving (Eq)

instance (Show a) => Show (Ast' a) where
    showsPrec = liftShowsPrec showsPrec showList

instance Show1 Ast' where
    liftShowsPrec sp _ d (AstNumber n) = showsUnaryWith showsPrec "" d n
    liftShowsPrec sp _ d (AstVar n coeff) = showsUnaryWith showsPrec (n ++ "^") d coeff
    liftShowsPrec sp _ _ (AstBinary op l r) = sp 0 l . showString " " . shows op . showString " " . sp 0 r
    liftShowsPrec sp _ _ (AstUnary op i) = shows op . showString " " . sp 0 i
    liftShowsPrec sp _ _ (AstGroup i) = showString "(" . sp 0 i . showString ")"

instance Eq1 Ast' where
    liftEq _ (AstNumber x) (AstNumber y) = x == y
    liftEq _ (AstVar x n) (AstVar y m) = x == y && n == m
    liftEq f (AstBinary opa la ra) (AstBinary opb lb rb) = opa == opb && f la lb && f ra rb
    liftEq f (AstUnary opa a) (AstUnary opb b) = opa == opb && f a b
    liftEq f (AstGroup a) (AstGroup b) = f a b
    liftEq _ _ _ = False

instance Functor Ast' where
    fmap _ (AstNumber n) = AstNumber n
    fmap _ (AstVar n c) = AstVar n c
    fmap f (AstBinary op lhs rhs) = AstBinary op (f lhs) (f rhs)

number :: Int -> Ast
number = Fix . AstNumber

var :: String -> Ast
var n = Fix $ AstVar n 1

binary :: Operator -> Ast -> Ast -> Ast
binary op lhs rhs = Fix $ AstBinary op lhs rhs

unary :: Operator -> Ast -> Ast
unary op = Fix . AstUnary op

group :: Ast -> Ast
group = Fix . AstGroup

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
