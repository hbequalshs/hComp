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

-- tests
test_initialize = initialize

test_push_top =
  let a = initialize
      b = push 1 a
  in top b

test_push_pop_top =
  let a = initialize
      b = push 1 a
      c = pop b
  in top c    

test_takeOneArg =
  let a = initialize
      b = push 1 a
  in takeOneArg b

test_takeTwoArgs =
  let a = initialize
      b = push 1 a
      c = push 2 b
  in takeTwoArgs c

test_empty_stack op = 
  let a = initialize
  in op a
