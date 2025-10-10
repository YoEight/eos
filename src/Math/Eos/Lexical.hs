module Math.Eos.Lexical where

import Data.Char(isLetter, isDigit, ord)
import Data.Functor (($>))
import Data.Int

import qualified Data.Attoparsec.Text as Atto
import Data.Text (Text)
import qualified Data.Text as T

import Math.Eos.Sym

parseSym :: Atto.Parser Sym
parseSym = Atto.peekChar' >>= go
  where
    go '(' = Atto.anyChar $> SymLParens
    go ')' = Atto.anyChar $> SymRParens
    go c
      | isLetter c = SymVar <$> Atto.takeWhile1 (\c -> isLetter c || isDigit c) 
      | isDigit c  = SymNumber . intoNumber <$> Atto.takeWhile1 isLetter
      | otherwise = parseOperator

parseOperator :: Atto.Parser Sym
parseOperator = fail "todo"

intoNumber :: Text -> Int
intoNumber = snd . T.foldr go (1, 0)
  where
    int c = ord c - ord '0'

    go c (base, acc) = (10 * base, int c * base)

