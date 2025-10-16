module Math.Eos.Mu where

import Data.Functor.Classes

newtype Mu f = Mu {unMu :: f (Mu f)}

muFold :: (Functor f) => (f a -> a) -> Mu f -> a
muFold f = f . fmap (muFold f) . unMu

instance (Eq1 f) => Eq (Mu f) where
    Mu x == Mu y = liftEq (==) x y
