-- tests stack
test_initialize0  = test_op0 id
test_pop0         = test_op0 pop
test_top0         = test_op0 top
test_takeOneArg0  = test_op0 takeOneArg
test_takeTwoArgs0 = test_op0 takeTwoArgs
test_read0         = test_op0 (read (-1))

test_push1        = test_op1 id
test_pop1         = test_op1 pop
test_top1         = test_op1 top
test_takeOneArg1  = test_op1 takeOneArg
test_takeTwoArgs1 = test_op1 takeTwoArgs
test_read1         = test_op1 (read 1)

test_push2        = test_op2 id
test_pop2         = test_op2 pop
test_top2         = test_op2 top
test_takeOneArg2  = test_op2 takeOneArg
test_takeTwoArgs2 = test_op2 takeTwoArgs
test_read2         = test_op2 (read 1)

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
