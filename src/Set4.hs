{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude
import Set2 (Maybe(..))

---

newtype Gen a = Gen { runGen :: Seed -> (a, Seed) }

evalGen :: Gen a -> Seed -> a
evalGen (Gen g) = fst . g

---

class Monad m where
  return :: a -> m a
  bind :: m a -> (a -> m b) -> m b


instance Monad Maybe where
  return x = Just x

  bind Nothing  _ = Nothing
  bind (Just x) f = f x


instance Monad [] where
  return x = [x]

  bind []     _ = []
  bind (x:xs) f = (f x) ++ bind xs f


instance Monad Gen where
  return x = Gen (\s -> (x, s))

  bind g f = Gen (\s -> let (x, s') = runGen g s in runGen (f x) s')

---

sequence :: Monad m => [m a] -> m [a]
sequence []     = return []
sequence (m:ms) = bind m (\a -> bind (sequence ms) (\b -> return (a:b)))

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb = bind ma (\a -> bind mb (\b -> return (f a b)))

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip bind

join :: Gen (Gen a) -> Gen a
join g = bind g id

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f ma mb mc = bind ma (\a -> bind mb (\b -> bind mc (\c -> return (f a b c))))

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma = bind ma (\a -> bind mf (\f -> return (f a)))
