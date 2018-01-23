{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

---

allPairs :: [a] -> [b] -> [(a,b)]
allPairs []     _      = []
allPairs _      []     = []
allPairs (x:xs) (y:ys) = (x,y) : (allPairs [x] ys) ++ (allPairs xs (y:ys))

---

data Card = Card Int String

instance Show Card where
  show (Card r s) = (show r) ++ s

allCards :: [Int] -> [String] -> [Card]
allCards []     _      = []
allCards _      []     = []
allCards (r:rs) (s:ss) = Card r s : allCards [r] ss ++ allCards rs (s:ss)

---

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs _ [] _ = []
allCombs _ _ [] = []
allCombs f (x:xs) (y:ys) = (f x y) : allCombs f [x] ys ++ allCombs f xs (y:ys)

allPairs2 :: [a] -> [b] -> [(a,b)]
allPairs2 = allCombs (,)

allCards2 :: [Int] -> [String] -> [Card]
allCards2 = allCombs Card

---

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 _ []     _      _      = []
allCombs3 _ _      []     _      = []
allCombs3 _ _      _      []     = []
allCombs3 f (x:xs) (y:ys) (z:zs) = (f x y z)
                                 : allCombs3 f [x] [y] zs
                                ++ allCombs3 f [x] ys (z:zs)
                                ++ allCombs3 f xs (y:ys) (z:zs)

---

combStep :: [a -> b] -> [a] -> [b]
combStep []     _      = []
combStep _      []     = []
combStep (f:fs) (x:xs) = (f x) : combStep [f] xs ++ combStep fs (x:xs)

allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs' f xs ys = combStep (map f xs) ys

allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' f xs ys zs = combStep (combStep (map f xs) ys) zs

allCombs4' :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
allCombs4' f ws xs ys zs = combStep (combStep (combStep (map f ws) xs) ys) zs

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
     && (allPairs2 [1,2] [3,4] == [(1,3),(1,4),(2,3),(2,4)])
     && (allPairs2 [1..3] [6..8] ==
         [(1,6),(1,7),(1,8),(2,6),(2,7),(2,8),(3,6),(3,7),(3,8)])
     && (show (allCards2 cardRanks cardSuits) ==
         "[2H,2D,2C,2S,3H,3D,3C,3S,4H,4D,4C,4S,5H,5D,5C,5S]")
     && (allCombs3 (,,) [1,2] [3,4] [5,6] ==
         [(1,3,5),(1,3,6),(1,4,5),(1,4,6),(2,3,5),(2,3,6),(2,4,5),(2,4,6)])
     && (allCombs (,) [1..5] [6..8] == allCombs' (,) [1..5] [6..8])
     && (allCombs3 (,,) [1,2] [3,4] [5,6] == allCombs3' (,,) [1,2] [3,4] [5,6])
