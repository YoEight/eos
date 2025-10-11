module Math.Eos.Mu where

newtype Mu f = Mu {unMu :: f (Mu f)}

muFold :: (Functor f) => (f a -> a) -> Mu f -> a
muFold f = f . fmap (muFold f) . unMu
