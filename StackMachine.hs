-- stack machine --
module StackMachine where

import Stack
import Operators

data StackMachine a = StackMachine { 
                       argumentStack :: Stack a
                    ,  addressStack  :: Stack a 
                    } deriving (Show)

initialize :: StackMachine a
initialize = StackMachine Stack.initialize Stack.initialize

push :: a -> StackMachine a -> StackMachine a
push a sM = StackMachine (Stack.push a arguments) addresses
    where arguments = argumentStack sM
          addresses = addressStack sM 

pop :: StackMachine a -> StackMachine a
pop sM = StackMachine (Stack.pop arguments) addresses
    where arguments = argumentStack sM
          addresses = addressStack sM 

-- operations
operateOneArg :: (a -> a) -> StackMachine a -> StackMachine a
operateOneArg op sM = StackMachine (op arg : stack) addresses
    where arguments = argumentStack sM
          addresses = addressStack sM
          (arg, stack) = Stack.takeOneArg arguments

operateTwoArgs :: (a -> a -> a) -> StackMachine a -> StackMachine a
operateTwoArgs op sM = StackMachine (arg1 `op` arg2 : stack) addresses
    where arguments = argumentStack sM
          addresses = addressStack sM
          (arg1, arg2, stack) = Stack.takeTwoArgs arguments

-- one argument
negate :: (Num a) => StackMachine a -> StackMachine a
negate = operateOneArg (-!)

--two argument
add, mul, sub :: (Num a) => StackMachine a -> StackMachine a
add  = operateTwoArgs (+)
mul = operateTwoArgs (*)
sub  = operateTwoArgs (-)

-- tests
-- one argument
test_push   = test_op1 id
test_pop    = test_op1 StackMachine.pop
test_negate = test_op1 StackMachine.negate

test_op1 op =
  let a = StackMachine.initialize
      b = StackMachine.push 1 a
  in op b    

-- two argument
test_add = test_op2 add
test_mul = test_op2 mul
test_sub = test_op2 sub

test_op2 op =
  let a = StackMachine.initialize
      b = StackMachine.push 10 a
      c = StackMachine.push 2  b
  in op c 
