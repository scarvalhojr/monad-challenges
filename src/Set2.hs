{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

---

data Maybe a = Nothing | Just a
  deriving Eq

instance Show a => Show (Maybe a) where
  show Nothing  = "Nothing"
  show (Just x) = "Just " ++ (show x)

---

headMay :: [a] -> Maybe a
headMay []     = Nothing
headMay (x:_) = Just x

tailMay :: [a] -> Maybe [a]
tailMay []     = Nothing
tailMay (_:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay x ((y,v):t)
  | x == y     = Just v
  | otherwise  = lookupMay x t

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay x y = Just (x / y)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay []     = Nothing
maximumMay (x:xs) = max' x (maximumMay xs)
  where max' x Nothing  = Just x
        max' x (Just y) = Just (max x y)

minimumMay :: Ord a => [a] -> Maybe a
minimumMay []     = Nothing
minimumMay (x:xs) = min' x (minimumMay xs)
  where min' x Nothing  = Just x
        min' x (Just y) = Just (min x y)

---

queryGreek :: GreekData -> String -> Maybe Double
queryGreek d k =
  case lookupMay k d of
    Nothing -> Nothing
    Just v  ->
      case headMay v of
        Nothing -> Nothing
        Just h  ->
          let Just t = tailMay v
           in case maximumMay t of
                Nothing -> Nothing
                Just mx -> divMay (fromIntegral mx) (fromIntegral h)

---

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain _ Nothing  = Nothing
chain f (Just x) = f x

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link Nothing  _ = Nothing
link (Just x) f = f x

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 d k = link m (\x -> link h (\y -> divMay (fromIntegral x) (fromIntegral y)))
  where v = lookupMay k d
        h = link v headMay
        t = link v tailMay
        m = link t maximumMay

---

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries xs k1 k2 =
  case lookupMay k1 xs of
    Nothing -> Nothing
    Just v1 ->
      case lookupMay k2 xs of
        Nothing -> Nothing
        Just v2 -> mkMaybe (v1 + v2)

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink f a b = link a (\x -> link b (\y -> mkMaybe (f x y)))

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 xs k1 k2 = yLink (+) v1 v2
  where v1 = lookupMay k1 xs
        v2 = lookupMay k2 xs

mkMaybe :: a -> Maybe a
mkMaybe x = Just x

---

tailProd :: Num a => [a] -> Maybe a
tailProd xs =
  case tailMay xs of
    Nothing -> Nothing
    Just t  -> Just (product t)

tailSum :: Num a => [a] -> Maybe a
tailSum xs =
  case tailMay xs of
    Nothing -> Nothing
    Just t  -> Just (sum t)

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe f m =
  case m of
    Nothing -> Nothing
    Just a  -> Just (f a)

tailProd2 :: Num a => [a] -> Maybe a
tailProd2 = transMaybe product . tailMay

tailSum2 :: Num a => [a] -> Maybe a
tailSum2 = transMaybe sum . tailMay

tailMax :: (Ord a) => [a] -> Maybe (Maybe a)
tailMax = transMaybe maximumMay . tailMay

tailMin :: (Ord a) => [a] -> Maybe (Maybe a)
tailMin = transMaybe minimumMay . tailMay

combine :: Maybe (Maybe a) -> Maybe a
combine Nothing         = Nothing
combine (Just Nothing)  = Nothing
combine (Just (Just x)) = Just x

---

tests :: Bool
tests = (queryGreek greekDataA "alpha" == Just 2.0)
     && (queryGreek greekDataA "beta" == Nothing)
     && (queryGreek greekDataA "gamma" == Just 3.3333333333333335)
     && (queryGreek greekDataA "delta" == Nothing)
     && (queryGreek greekDataA "zeta" == Nothing)
     && (queryGreek greekDataB "rho" == Nothing)
     && (queryGreek greekDataB "phi" == Just 0.24528301886792453)
     && (queryGreek greekDataB "chi" == Just 9.095238095238095)
     && (queryGreek greekDataB "psi" == Nothing)
     && (queryGreek greekDataB "omega" == Just 24.0)
     && (queryGreek2 greekDataA "alpha" == Just 2.0)
     && (queryGreek2 greekDataA "beta" == Nothing)
     && (queryGreek2 greekDataA "gamma" == Just 3.3333333333333335)
     && (queryGreek2 greekDataA "delta" == Nothing)
     && (queryGreek2 greekDataA "zeta" == Nothing)
     && (queryGreek2 greekDataB "rho" == Nothing)
     && (queryGreek2 greekDataB "phi" == Just 0.24528301886792453)
     && (queryGreek2 greekDataB "chi" == Just 9.095238095238095)
     && (queryGreek2 greekDataB "psi" == Nothing)
     && (queryGreek2 greekDataB "omega" == Just 24.0)
