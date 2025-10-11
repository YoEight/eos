module Math.Eos.Lexical (tokenize, runTokenize, parseSym, parseSyms) where

import Prelude hiding (takeWhile)

import Control.Applicative ((<|>))
import Control.Monad.Except (throwError)
import Control.Monad.State
import Data.Attoparsec.Text qualified as Atto
import Data.Char (isDigit, isLetter, ord)
import Data.Functor (($>))
import Data.Int
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Math.Eos.Sym

data Internal
    = Internal
    { internalPos :: !Pos
    , internalInput :: !String
    }
    deriving (Show, Eq)

type Tokenize a = StateT Internal (Either String) a

tokenize :: String -> Either String [Sym]
tokenize = runTokenize parseSyms

runTokenize :: Tokenize a -> String -> Either String a
runTokenize action input = fst <$> runStateT action start
  where
    start = Internal{internalPos = posDefault, internalInput = input}

lookAhead :: Tokenize (Maybe Char)
lookAhead = skipSpaces *> gets (go . internalInput)
  where
    go (x : _) = Just x
    go _ = Nothing

lookAheadOrBail :: Tokenize Char
lookAheadOrBail = go =<< lookAhead
  where
    go (Just x) = pure x
    go Nothing = throwError "unexpected end of input"

shift :: Tokenize Char
shift = do
    skipSpaces
    go =<< get
  where
    go s = case internalInput s of
        (x : xs) -> x <$ put s{internalPos = posColIncr $ internalPos s, internalInput = xs}
        _ -> throwError "unexpected en of input"

takeWhile :: (Char -> Bool) -> Tokenize String
takeWhile f = go
  where
    go = do
        opt <- lookAhead
        case opt of
            Nothing -> pure []
            Just x
                | f x -> shift *> fmap (x :) go
                | otherwise -> pure []

position :: Tokenize Pos
position = gets internalPos

skipSpaces :: Tokenize ()
skipSpaces = modify go
  where
    go s = case internalInput s of
        (' ' : xs) -> go s{internalInput = xs, internalPos = posColIncr $ internalPos s}
        _ -> s

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
parseVar = SymVar <$> takeWhile isLetter

parseNumber :: Tokenize Sym
parseNumber = SymNumber . intoNumber <$> takeWhile isDigit

parseOperator :: Tokenize Sym
parseOperator = do
    pos <- position
    go pos =<< shift
  where
    go _ '-' = pure $ SymOp Sub
    go _ '*' = pure $ SymOp Mul
    go _ '+' = pure $ SymOp Add
    go _ '/' = pure $ SymOp Div
    go _ '=' = pure $ SymOp Equal
    go p x = throwError $ show p ++ ": unexpected character '" ++ show x ++ "'"

intoNumber :: String -> Int
intoNumber = snd . foldr go (1, 0)
  where
    int c = ord c - ord '0'

    go c (base, acc) = (10 * base, int c * base)
