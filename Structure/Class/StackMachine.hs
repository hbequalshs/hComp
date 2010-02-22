module Structure.Class.StackMachine 
  (StackMachine(..)) where

  class StackMachine sm where
    load  :: [a] -> sm a b    
    readM :: sm a b -> Maybe a
    next  :: sm a b -> sm a b

    top   :: sm a b -> b

    push  :: b -> sm a b -> sm a b 
    read  :: (Ord t, Num t) => t -> sm a b -> sm a b
    write :: (Ord t, Num t) => t -> sm a b -> sm a b
    store :: (Ord t, Num t) => t -> sm a b -> sm a b

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

    jump  :: (Ord t, Num t) => t -> sm a b -> sm a b 
    jnon0 :: (Ord t, Num t, Num b) => t -> sm a b -> (t, sm a b)
    jump0 :: (Ord t, Num t, Num b) => t -> sm a b -> (t, sm a b)

    call  :: (Ord t, Num t) => b -> t -> sm a b -> sm a b
    ret   :: (Ord b, Num b) => b -> sm a b -> (b, sm a b)
