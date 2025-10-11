module Math.Eos.Parser (
    Parser,
    runParser,
    lookAhead,
    lookAheadOrBail,
    shift,
    takeWhile,
) where

import Control.Monad.Except (throwError)
import Control.Monad.State
import Prelude hiding (takeWhile)

type Parser i a = StateT [i] (Either String) a

runParser :: Parser i a -> [i] -> Either String a
runParser = evalStateT

lookAhead :: Parser i (Maybe i)
lookAhead = gets go
  where
    go (x : _) = Just x
    go _ = Nothing

lookAheadOrBail :: Parser i i
lookAheadOrBail = go =<< lookAhead
  where
    go (Just x) = pure x
    go Nothing = throwError "unexpected end of input"

shift :: Parser i i
shift = do
    go =<< get
  where
    go (x : xs) = x <$ put xs
    go _ = throwError "unexpected en of input"

takeWhile :: (i -> Bool) -> Parser i [i]
takeWhile f = go
  where
    go = do
        opt <- lookAhead
        case opt of
            Nothing -> pure []
            Just x
                | f x -> shift *> fmap (x :) go
                | otherwise -> pure []
