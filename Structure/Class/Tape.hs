module Structure.Class.Tape 
  (Tape(..)) where

  class Tape t where
    load :: [a] -> t a

    move :: (Ord a, Num a) => a -> t b -> t b
