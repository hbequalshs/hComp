module StackMachine (StackMachine(..)) where

  class StackMachine sm where
    load  :: [a] -> sm a b    

    push  :: b -> sm a b -> sm a b 
    write :: (Ord t, Num t) => t -> sm a b -> sm a b
    read  :: (Ord t, Num t) => t -> sm a b -> sm a b

    pop   :: sm a b -> sm a b

    neg   :: (Num b) => sm a b -> sm a b

    add   :: (Num b) => sm a b -> sm a b 
    mul   :: (Num b) => sm a b -> sm a b 
    sub   :: (Num b) => sm a b -> sm a b 

    div   :: sm a Double -> sm a Double

    eq    :: sm a Double -> sm a Double   
    neq   :: sm a Double -> sm a Double
    lt    :: sm a Double -> sm a Double
    leq   :: sm a Double -> sm a Double
    gt    :: sm a Double -> sm a Double    
    geq   :: sm a Double -> sm a Double
