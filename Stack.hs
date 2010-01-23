module Stack (Stack(..)) where

  class Stack s where
    empty     :: s a
    isEmpty   :: s a -> Bool

    push      :: a -> s a -> s a
    pop       :: s a -> s a
    top       :: s a -> a

    read      :: (Ord t, Num t) => t -> s a -> s a
    write     :: (Ord t, Num t) => t -> s a -> s a
    
    take1Arg  :: s a -> (a, s a)
    take2Args :: s a -> (a, a, s a)
