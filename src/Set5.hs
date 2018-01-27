{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set5 where

import MCPrelude
import Set2 (Maybe(..), headMay, tailMay, lookupMay, divMay, maximumMay,
  minimumMay)
import Set3 (Card(..))
import Set4 (Gen(..), evalGen)

---

class Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a

    fail :: String -> m a
    fail = undefined

---

instance Monad Gen where
  return x = Gen (\s -> (x, s))

  g >>= f = Gen (\s -> let (x, s') = runGen g s in runGen (f x) s')

---

makeRandom :: Gen Integer
makeRandom = Gen rand

fiveRands :: Gen [Integer]
fiveRands = do
  r1 <- makeRandom
  r2 <- makeRandom
  r3 <- makeRandom
  r4 <- makeRandom
  r5 <- makeRandom
  return [r1, r2, r3, r4, r5]

randLetter :: Gen Char
randLetter = do
  i <- makeRandom
  return (toLetter i)

randString3 :: Gen String
randString3 = do
  c1 <- randLetter
  c2 <- randLetter
  c3 <- randLetter
  return [c1, c2, c3]

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair ga gb = do
  a <- ga
  b <- gb
  return (a, b)

---

instance Monad Maybe where
  return x = Just x

  Nothing >>= _ = Nothing
  Just x  >>= f = f x

queryGreek :: GreekData -> String -> Maybe Double
queryGreek d k = do
  l <- lookupMay k d
  h <- headMay l
  m <- tailMay l >>= maximumMay
  divMay (fromIntegral m) (fromIntegral h)

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries xs k1 k2 = do
  s1 <- lookupMay k1 xs
  s2 <- lookupMay k2 xs
  return (s1 + s2)

tailProd :: Num a => [a] -> Maybe a
tailProd xs = do
  t <- tailMay xs
  return (product t)

tailSum :: Num a => [a] -> Maybe a
tailSum xs = do
  t <- tailMay xs
  return (sum t)

tailMax :: Ord a => [a] -> Maybe a
tailMax xs = do
  t <- tailMay xs
  maximumMay t

---

instance Monad [] where
  return x = [x]

  []     >>= _ = []
  (x:xs) >>= f = (f x) ++ (xs >>= f)

allPairs :: [a] -> [b] -> [(a,b)]
allPairs xs ys = do
  x <- xs
  y <- ys
  return (x,y)

allCards :: [Int] -> [String] -> [Card]
allCards ns ks = do
  (n, k) <- allPairs ns ks
  return (Card n k)

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f xs ys zs = do
  x <- xs
  y <- ys
  z <- zs
  return (f x y z)

---

tests :: Bool
tests = (product (evalGen fiveRands (mkSeed 1))
         == 8681089573064486461641871805074254223660)
     && (evalGen randString3 (mkSeed 1) == "lrf")
     && (evalGen (generalPair makeRandom randString3) (mkSeed 1)
         == (16807,"rfk"))
     && (queryGreek greekDataA "alpha" == Just 2.0)
     && (queryGreek greekDataA "beta" == Nothing)
     && (queryGreek greekDataA "gamma" == Just 3.3333333333333335)
     && (queryGreek greekDataA "delta" == Nothing)
     && (queryGreek greekDataA "zeta" == Nothing)
     && (queryGreek greekDataB "rho" == Nothing)
     && (queryGreek greekDataB "phi" == Just 0.24528301886792453)
     && (queryGreek greekDataB "chi" == Just 9.095238095238095)
     && (queryGreek greekDataB "psi" == Nothing)
     && (queryGreek greekDataB "omega" == Just 24.0)
     && (addSalaries salaries "alice" "bob" == Just 195000)
     && (addSalaries salaries "alice" "nobody" == Nothing)
     && (addSalaries salaries "nobody" "bob" == Nothing)
     && (addSalaries salaries "nobody" "nobody" == Nothing)
     && (show (allCards cardRanks cardSuits)
         == "[2H,2D,2C,2S,3H,3D,3C,3S,4H,4D,4C,4S,5H,5D,5C,5S]")
     && (allCombs3 (,,) [1,2] [3,4] [5,6]
         == [(1,3,5),(1,3,6),(1,4,5),(1,4,6),(2,3,5),(2,3,6),(2,4,5),(2,4,6)])
