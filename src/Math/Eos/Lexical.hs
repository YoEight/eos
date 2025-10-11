module Math.Eos.Lexical (tokenize) where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Monad.Except (throwError)
import Control.Monad.State
import Data.Attoparsec.Text qualified as Atto
import Data.Char (isDigit, isLetter, ord)
import Data.Functor (($>))
import Data.Int
import Data.Text (Text)
import Data.Text qualified as T
import Math.Eos.Sym

data Internal
    = Internal
    { internalLine :: !Int
    , internalCol :: !Int
    , internalInput :: !String
    }
    deriving (Show, Eq)

tokenize :: String -> Either String [Sym]
tokenize input = fst <$> runStateT parseSyms start
  where
    start = Internal{internalLine = 1, internalCol = 1, internalInput = input}

type Tokenize a = StateT Internal (Either String) a

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
        (x : xs) -> x <$ put s{internalCol = internalCol s + 1, internalInput = xs}
        _ -> throwError "unexpected en of input"

position :: Tokenize (Int, Int)
position = gets (internalLine &&& internalCol)

skipSpaces :: Tokenize ()
skipSpaces = modify go
  where
    go s = case internalInput s of
        (' ' : xs) -> go s{internalInput = xs, internalCol = internalCol s + 1}
        _ -> s

parseSyms :: Tokenize [Sym]
parseSyms = (:) <$> parseSym <*> parseSyms

parseSym :: Tokenize Sym
parseSym = go =<< lookAheadOrBail
  where
    go '(' = pure SymLParens
    go ')' = pure SymRParens
    go c
        | isLetter c = parseVar
        | isDigit c = parseNumer
        | otherwise = parseOperator

-- tokenize :: Text -> Either String [Sym]
-- tokenize = Atto.parseOnly (Atto.many1 parseSym <* Atto.endOfInput)

-- parseSym :: Atto.Parser Sym
-- parseSym = do
--     Atto.skipSpace
--     ch <- Atto.peekChar'
--     go ch
--   where
--     go '(' = Atto.take 1 $> SymLParens
--     go ')' = Atto.take 1 $> SymRParens
--     go c
--         | isLetter c = SymVar <$> Atto.takeWhile1 (\c -> isLetter c || isDigit c)
--         | isDigit c = SymNumber . intoNumber <$> Atto.takeWhile1 isLetter
--         | otherwise = SymOp <$> parseOperator

-- parseOperator :: Atto.Parser Operator
-- parseOperator =
--     Atto.char '+' $> Add
--         <|> Atto.char '-' $> Sub
--         <|> Atto.char '*' $> Mul
--         <|> Atto.char '/' $> Div
--         <|> Atto.char '^' $> Exp
--         <|> Atto.char '=' $> Equal

-- intoNumber :: Text -> Int
-- intoNumber = snd . T.foldr go (1, 0)
--   where
--     int c = ord c - ord '0'

--     go c (base, acc) = (10 * base, int c * base)
