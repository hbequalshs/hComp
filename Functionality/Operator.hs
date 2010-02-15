{-# LANGUAGE NoMonomorphismRestriction #-}

module Functionality.Operator where

import Prelude hiding (compare)

-- one argument
(-!) :: (Num a) => a -> a
(-!) = negate 

-- two argument
division :: (Fractional a) => (a -> a -> a)
division _ 0 = 0.0
division a b = a/b
(/!) = division
infixl 7 /!

compare :: a -> a -> (b -> b -> Bool) -> b -> b -> a
compare true false op a b
  | a `op` b  = true
  | otherwise = false

compareOneZero :: (a -> a -> Bool) -> a -> a -> Double
compareOneZero = compare 1.0 0.0

equal, less, lessEqual, greater, greaterEqual :: (Ord a) => a -> a -> Double

equal = compareOneZero (==)
(==!) = equal
infix 4 ==!

notequal = compareOneZero (/=)
(/=!)    = notequal
infix 4 /=!

less = compareOneZero (<)
(<!) = less
infix 4 <!

lessEqual = compareOneZero (<=)
(<=!)     = lessEqual
infix 4 <=!

greater = compareOneZero (>)
(>!)    = greater
infix 4 >!

greaterEqual = compareOneZero (>=)
(>=!)        = greaterEqual
infix 4 >=!
