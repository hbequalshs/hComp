-- A simple stack module --

module Register (Register) where

  import Prelude hiding (read)
  import Stack

  data Register a = Reg [a]
                    deriving (Show)

  getAndPush :: (Num t) => t -> [a] -> [a] -> [a] 
  getAndPush 0 xss@(x : xs) ys = x : (revapp xss ys)
  getAndPush n (x : xs)     ys = getAndPush (n - 1) xs (x : ys)
  getAndPush _ _            _  = error "read(getAndPush): register empty"
  
  putInside :: (Num t) => t -> a -> [a] -> [a] -> [a]
  putInside 0 x0 xs       ys = revapp ys (x0 : xs)   
  putInside n x0 (x : xs) ys = putInside (n - 1) x0 xs (x : ys)
  putInside _ _  _        _  = error "write(putInside): register empty"

  revapp :: [a] -> [a] -> [a]
  revapp xs (y : ys) = revapp (y : xs) ys
  revapp xs _        = xs
  
  instance Stack Register where
    empty            = Reg []
    isEmpty (Reg rr) = null rr 

    push r (Reg rr) = Reg (r : rr)

    pop (Reg (r : rr)) = Reg rr
    pop _              = error "pop: register empty"

    top (Reg (r : rr)) = r
    top _              = error "top: register empty"

    read n (Reg rr)  
      | n < 0     = error "read: argument must be positive"
      | otherwise = Reg $ getAndPush n rr []

    write n (Reg (r : rr))
      | n < 0     = error "write: argument must be positive"
      | otherwise = Reg $ putInside n r rr [] 
    write _ _     = error "write: nothing to write"

    take1Arg (Reg (r : rr)) = (r, Reg rr)
    take1Arg _              = error "takeOne: register empty"

    take2Args (Reg (r2 : r1 : rr)) = (r1, r2, Reg rr)
    take2Args _                    = error "takeTwo: register empty"
