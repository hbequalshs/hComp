module Memory (Memory) where

  import Tape 

  data Memory a = Mem [a] [a]
                  deriving (Show)

  next (Mem _  []      ) = error "move: out of memory range"
  next (Mem fs (r : rs)) = Mem (r : fs) rs 

  swap (Mem f s) = Mem s f

  moveF 0 mem = mem 
  moveF n mem = moveF (n - 1) $ next mem

  moveB 0 mem = swap mem
  moveB n mem = moveB (n - 1) $ next mem

  instance Tape Memory where
    load xs = Mem [] xs 

    move n mem 
        | n < 0     = moveB (negate n) swappedMem
        | n == 0    = mem 
        | otherwise = moveF n mem 
      where swappedMem = swap mem
