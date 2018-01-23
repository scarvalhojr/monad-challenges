{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

---

type Gen a = Seed -> (a, Seed)

mkRands :: Gen a -> Int -> Integer -> [a]
mkRands f n = take n . map fst . iterate (f . snd) . f . mkSeed

fiveRands :: [Integer]
fiveRands = mkRands rand 5 1

---

randLetter :: Gen Char
randLetter s = (toLetter rnd, nxt)
  where (rnd, nxt) = rand s

randString3 :: String
randString3 = mkRands randLetter 3 1

---

generalA :: (a -> b) -> Gen a -> Gen b
generalA f g s = (f rnd, nxt)
  where (rnd, nxt) = g s

randEven :: Gen Integer
randEven = generalA (*2) rand

randOdd :: Gen Integer
randOdd = generalA (+1) randEven

randTen :: Gen Integer
randTen = generalA (*10) rand

---

generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair ga gb s = ((a, b), s'')
  where (a, s')  = ga s
        (b, s'') = gb s'

randPair :: Gen (Char, Integer)
randPair = generalPair randLetter rand

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f ga gb s = (f a b, s'')
  where (a, s')  = ga s
        (b, s'') = gb s'

generalPair2 :: Gen a -> Gen b -> Gen (a,b)
generalPair2 = generalB (,)

randPair2 :: Gen (Char, Integer)
randPair2 = generalPair2 randLetter rand

---

repRandom :: [Gen a] -> Gen [a]
repRandom []     s = ([], s)
repRandom (g:gs) s = (x : xs, s'')
  where (x, s')   = g s
        (xs, s'') = repRandom gs s'

---

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo g f s = (f a) s'
  where (a, s') = g s

mkGen :: a -> Gen a
mkGen x s = (x, s)

---

-- Back from Set 4.2

generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB2 f ga gb = genTwo ga (\x -> genTwo gb (\y -> mkGen (f x y)))

repRandom' :: [Gen a] -> Gen [a]
repRandom' []     = mkGen []
repRandom' (g:gs) = genTwo g (\x -> genTwo (repRandom' gs) (\y -> mkGen (x : y)))

---


tests :: Bool
tests = (product fiveRands == 8681089573064486461641871805074254223660)
     && (randString3 == "lrf")
     && (product (map (\f -> fst (f (mkSeed 1))) [randEven, randOdd, randTen])
         == 189908109902700)
     && (fst (randPair (mkSeed 1)) == ('l',282475249))
     && (randPair (mkSeed 1) == randPair2 (mkSeed 1))
     && (fst (repRandom (replicate 3 randLetter) (mkSeed 1)) == "lrf")
