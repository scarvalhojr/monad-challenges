{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2b where

import MCPrelude
import Set2 (Maybe(..), headMay, tailMay, lookupMay, divMay, maximumMay,
  minimumMay)
import Set4

---

queryGreek :: GreekData -> String -> Maybe Double
queryGreek d k = bind m (\x -> bind h (\y -> divMay (fromIntegral x) (fromIntegral y)))
  where l = lookupMay k d
        m = bind (bind l tailMay) maximumMay
        h = bind l headMay

---

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries xs k1 k2 = liftM2 (+) v1 v2
  where v1 = lookupMay k1 xs
        v2 = lookupMay k2 xs

---

tailProd :: Num a => [a] -> Maybe a
tailProd xs = bind (tailMay xs) (return . product)

tailSum :: Num a => [a] -> Maybe a
tailSum xs = bind (tailMay xs) (return . sum)

tailMax :: (Ord a) => [a] -> Maybe a
tailMax xs = bind (tailMay xs) maximumMay

tailMin :: (Ord a) => [a] -> Maybe a
tailMin xs = bind (tailMay xs) minimumMay

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
