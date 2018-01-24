{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude
import Set4

---

allPairs :: [a] -> [b] -> [(a,b)]
allPairs = liftM2 (,)

---

data Card = Card Int String

instance Show Card where
  show (Card r s) = (show r) ++ s

allCards :: [Int] -> [String] -> [Card]
allCards = liftM2 Card

---

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs = liftM2

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 = liftM3

---

tests :: Bool
tests = (allPairs [1,2] [3,4] == [(1,3),(1,4),(2,3),(2,4)])
     && (allPairs [1..3] [6..8] ==
         [(1,6),(1,7),(1,8),(2,6),(2,7),(2,8),(3,6),(3,7),(3,8)])
     && (allPairs cardRanks cardSuits ==
         [(2,"H"),(2,"D"),(2,"C"),(2,"S"),(3,"H"),(3,"D"),(3,"C"),(3,"S"),
          (4,"H"),(4,"D"),(4,"C"),(4,"S"),(5,"H"),(5,"D"),(5,"C"),(5,"S")])
     && (show (allCards cardRanks cardSuits) ==
         "[2H,2D,2C,2S,3H,3D,3C,3S,4H,4D,4C,4S,5H,5D,5C,5S]")
     && (allCombs3 (,,) [1,2] [3,4] [5,6] ==
         [(1,3,5),(1,3,6),(1,4,5),(1,4,6),(2,3,5),(2,3,6),(2,4,5),(2,4,6)])
