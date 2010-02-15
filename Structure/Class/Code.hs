module Structure.Class.Code
  (Code(..)) where

  class Code c where
    parse :: String -> (Maybe (c String), Maybe String) 
    parseAll :: [String] -> ([c String], [String])
