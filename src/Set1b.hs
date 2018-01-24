{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1b where

import MCPrelude
import Set4

---

mkRands :: Gen a -> Int -> Integer -> [a]
mkRands g n = evalGen (sequence (replicate n g)) . mkSeed

randInt :: Gen Integer
randInt = Gen rand

fiveRands :: [Integer]
fiveRands = mkRands randInt 5 1

---

randLetter :: Gen Char
randLetter = bind randInt (return . toLetter)

randString3 :: String
randString3 = mkRands randLetter 3 1

---

randEven :: Gen Integer
randEven = bind randInt (return . (*2))

randOdd :: Gen Integer
randOdd = bind randEven (return . (+1))

randTen :: Gen Integer
randTen = bind randInt (return . (*10))

---

randPair :: Gen (Char, Integer)
randPair = liftM2 (,) randLetter randInt

---

tests :: Bool
tests = (product fiveRands == 8681089573064486461641871805074254223660)
     && (randString3 == "lrf")
     && (product (map (`evalGen` (mkSeed 1)) [randEven, randOdd, randTen])
         == 189908109902700)
     && (evalGen randPair (mkSeed 1) == ('l',282475249))
     && (evalGen (sequence (replicate 3 randLetter)) (mkSeed 1) == "lrf")
