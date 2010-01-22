-- A simple stack module --

{-
module Stack 
    ( 
      initialize
    , push
    , pop
    , top
    , takeTwo
    )where
-}

module Stack where

import Prelude hiding (read)

type Stack a = [a]

initialize :: Stack a
initialize = []

push :: a -> Stack a -> Stack a
push s stack = s : stack

pop :: Stack a -> Stack a
pop stack
  | null stack = error "pop: stack empty"
  | otherwise  = tail stack

top :: Stack a -> a
top stack
  | null stack = error "top: stack empty"
  | otherwise  = head stack

takeOneArg :: Stack a -> (a, Stack a)
takeOneArg (s1:stack) = (s1, stack)
takeOneArg _          = error "takeOne: stack empty"

takeTwoArgs :: Stack a -> (a, a, Stack a)
takeTwoArgs (s2:s1:stack) = (s1, s2, stack)
takeTwoArgs _             = error "takeTwo: stack empty"

read :: (Ord a, Num a) => a -> Stack b -> (b, Stack b)
read n stack 
  | n < 0     = error "read: argument must be positive"
  | otherwise = getAndRemove n stack []

getAndRemove :: (Num a) => a -> [b] -> [b] -> (b, [b]) 
getAndRemove 0 (s:stackEnd) stackStart = (s, revapp stackStart stackEnd)
getAndRemove n (s:stackEnd) stackStart = getAndRemove (n-1) stackEnd (s:stackStart)
getAndRemove _ _            _          = error "read(getAndRemove): stack empty"

write :: (Ord a, Num a) => a -> Stack b -> Stack b
write n (s0:stack)
  | n < 0     = error "write: argument must be positive"
  | n == 0    = s0 : stack
  | otherwise = putInside n s0 stack [] 
write _ []    = error "write: nothing to write"

putInside :: (Num a) => a -> b -> [b] -> [b] -> [b]
putInside 0 s0 stackEnd stackStart     = revapp stackStart (s0:stackEnd)   
putInside n s0 (s:stackEnd) stackStart = putInside (n-1) s0 stackEnd (s:stackStart)
putInside _ _  _            _          = error "write: stack empty"

revapp :: [a] -> [a] -> [a]
revapp (x:xs) ys = revapp xs (x:ys)
revapp _      ys = ys

-- tests
test_initialize0  = test_op0 id
test_pop0         = test_op0 pop
test_top0         = test_op0 top
test_takeOneArg0  = test_op0 takeOneArg
test_takeTwoArgs0 = test_op0 takeTwoArgs
test_get0         = test_op0 (read (-1))

test_push1        = test_op1 id
test_pop1         = test_op1 pop
test_top1         = test_op1 top
test_takeOneArg1  = test_op1 takeOneArg
test_takeTwoArgs1 = test_op1 takeTwoArgs
test_get1         = test_op1 (read 1)

test_push2        = test_op2 id
test_pop2         = test_op2 pop
test_top2         = test_op2 top
test_takeOneArg2  = test_op2 takeOneArg
test_takeTwoArgs2 = test_op2 takeTwoArgs
test_get2         = test_op2 (read 1)

test_op0 op =
  let a = initialize
  in op a

test_op1 op =
  let a = initialize
      b = push 1 a
  in op b

test_op2 op =
  let a = initialize
      b = push 1 a
      c = push 2 b
  in op c
