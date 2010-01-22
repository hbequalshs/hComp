-- stack machine --
module StackMachine where

import Prelude hiding (div, read)
import qualified Stack(Stack, push, pop, initialize, takeOneArg, takeTwoArgs, read, write)
import Operators

data StackMachine a = StackMachine { 
                       argumentStack :: Stack.Stack a
                    ,  addressStack  :: Stack.Stack a 
                    } deriving (Show)

initialize :: StackMachine a
initialize = StackMachine Stack.initialize Stack.initialize

push :: a -> StackMachine a -> StackMachine a
push  = simpleOpOneArg Stack.push

write :: (Ord a, Num a) => a -> StackMachine a -> StackMachine a
write = simpleOpOneArg Stack.write

simpleOpOneArg :: (t -> Stack.Stack a -> Stack.Stack a) -> t -> StackMachine a -> StackMachine a
simpleOpOneArg op a sM = StackMachine (op a arguments) addresses
    where arguments = argumentStack sM
          addresses = addressStack sM 

pop :: StackMachine a -> StackMachine a
pop sM = StackMachine (Stack.pop arguments) addresses
    where arguments = argumentStack sM
          addresses = addressStack sM 

-- operations
operateOneArg :: (a -> a) -> (Stack.Stack a -> (a, Stack.Stack a)) -> StackMachine a -> StackMachine a
operateOneArg op exOp sM = StackMachine (op arg : stack) addresses
    where arguments = argumentStack sM
          addresses = addressStack sM
          (arg, stack) = exOp arguments

operateTwoArgs :: (a -> a -> a) -> StackMachine a -> StackMachine a
operateTwoArgs op sM = StackMachine (arg1 `op` arg2 : stack) addresses
    where arguments = argumentStack sM
          addresses = addressStack sM
          (arg1, arg2, stack) = Stack.takeTwoArgs arguments

-- one argument
neg :: (Num a) => StackMachine a -> StackMachine a
neg = operateOneArg (-!) Stack.takeOneArg 

read :: (Ord a, Num a) => a -> StackMachine b -> StackMachine b
read n = operateOneArg id $ Stack.read n

--two argument
add, mul, sub :: (Num a) => StackMachine a -> StackMachine a
add = operateTwoArgs (+)
mul = operateTwoArgs (*)
sub = operateTwoArgs (-)

div :: StackMachine Double -> StackMachine Double
div = operateTwoArgs (/!)

eq, lt, leq, gt, geq :: StackMachine Double -> StackMachine Double
eq  = operateTwoArgs (==!)
neq = operateTwoArgs (/=!)
lt  = operateTwoArgs (<!)
leq = operateTwoArgs (<=!)
gt  = operateTwoArgs (>!)
geq = operateTwoArgs (>=!)

-- tests
-- one argument
test_push = test_op1 id
test_pop  = test_op1 pop
test_neg  = test_op1 neg

test_op1 op =
  let a = initialize
      b = push 1 a
  in op b    

-- two argument
test_add   = test_op2 add
test_mul   = test_op2 mul
test_sub   = test_op2 sub
test_div   = test_op2 div
test_eq    = test_op2 eq
test_neq   = test_op2 neq
test_lt    = test_op2 lt
test_leq   = test_op2 leq
test_gt    = test_op2 gt
test_geq   = test_op2 geq
test_read  = test_op2 (read 1)
test_write = test_op2 (write 1)

test_op2 op =
  let a = initialize
      b = push 10 a
      c = push 2  b
  in op c 
