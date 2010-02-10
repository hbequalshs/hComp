module Code(Code(..)) where

  class Code c where
    parse :: String -> c
