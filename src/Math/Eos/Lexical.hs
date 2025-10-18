module Math.Eos.Lexical (tokenize, runTokenize, parseSym, parseSyms) where

import Prelude hiding (takeWhile)

import Control.Applicative ((<|>))
import Control.Monad.Except (throwError)
import Control.Monad.State
import Data.Char (isDigit, isLetter, isSpace, ord)
import Data.Functor (void, ($>))
import Data.Int
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T

import Math.Eos.Parser qualified as P
import Math.Eos.Sym

type Tokenize a = P.Parser Char a

tokenize :: String -> Either String [Sym]
tokenize = runTokenize parseSyms

runTokenize :: Tokenize a -> String -> Either String a
runTokenize = P.runParser

skipSpaces :: Tokenize ()
skipSpaces = void $ P.takeWhile isSpace

lookAhead :: Tokenize (Maybe Char)
lookAhead = skipSpaces *> P.lookAhead

lookAheadOrBail :: Tokenize Char
lookAheadOrBail = skipSpaces *> P.lookAheadOrBail

shift :: Tokenize Char
shift = skipSpaces *> P.shift

parseSyms :: Tokenize [Sym]
parseSyms = (:) <$> parseSym <*> go
  where
    go = do
        opt <- lookAhead
        if isJust opt
            then parseSyms
            else pure []

parseSym :: Tokenize Sym
parseSym = go =<< lookAheadOrBail
  where
    go '(' = SymLParens <$ shift
    go ')' = SymRParens <$ shift
    go c
        | isLetter c = parseVar
        | isDigit c = parseNumber
        | otherwise = parseOperator

parseVar :: Tokenize Sym
parseVar = SymVar <$> P.takeWhile isLetter

parseNumber :: Tokenize Sym
parseNumber = SymNumber . intoNumber <$> P.takeWhile isDigit

parseOperator :: Tokenize Sym
parseOperator = go =<< shift
  where
    go '-' = pure $ SymOp Sub
    go '*' = pure $ SymOp Mul
    go '+' = pure $ SymOp Add
    go '/' = pure $ SymOp Div
    go '^' = pure $ SymOp Exp
    go '=' = pure $ SymOp Equal
    go x = throwError $ ": unexpected character '" ++ show x ++ "'"

intoNumber :: String -> Int
intoNumber = snd . foldr go (1, 0)
  where
    int c = ord c - ord '0'

    go c (base, acc) = (10 * base, int c * base)
