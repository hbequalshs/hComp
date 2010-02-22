module Structure.Instance.Assembler
  ( Assembler, show, load, readM, next, top, push
  , write, read, pop, neg, add, mul, sub, div, eq
  , neq, lt, leq, gt, geq, jump, jnon0, jump0 
  , store, ret, call ) where

-- {{{ imports
  import Prelude hiding (div, read)
  import qualified Structure.Instance.Memory   as Mem
  import qualified Structure.Instance.Register as Reg
  import Structure.Class.StackMachine
  import Functionality.Operator
-- }}}
-- {{{ data and type declarations
  data Assembler a b = ASM { 
                           memory :: Memory a
                        ,  argRR  :: Register b
                        ,  adrRR  :: Register b 
                        } deriving (Show)

  type Memory   a = Mem.Memory a
  type Register a = Reg.Register a
-- }}}
-- {{{ local functions
  simpleOp1Arg :: (t -> Register b -> Register b) -> t -> Assembler a b -> Assembler a b 
  simpleOp1Arg op a asm = ASM mem (op a args) adrs
      where mem  = memory asm
            args = argRR  asm
            adrs = adrRR  asm

  operate1Arg :: (a -> a) -> (Register a -> (a, Register a)) -> Assembler b a -> Assembler b a
  operate1Arg op exOp asm = ASM mem (Reg.push (op a) as) adrs
      where mem     = memory asm
            args    = argRR  asm
            adrs    = adrRR  asm
            (a, as) = exOp args

  operate2Args :: (a -> a -> a) -> Assembler b a -> Assembler b a
  operate2Args op asm = ASM mem (Reg.push(a1 `op` a2) as) adrs
      where mem          = memory asm
            args         = argRR  asm
            adrs         = adrRR  asm
            (a1, a2, as) = Reg.take2Args args

  
  moveIf :: (Ord t, Num t) => (a -> Bool) -> t -> Assembler b a -> (t, Assembler b a)
  moveIf op n asm 
    | op a      = (n, ASM (Mem.move n mem) as adrs)
    | otherwise = (1, ASM (Mem.move 1 mem) as adrs)
      where mem     = memory asm
            args    = argRR  asm
            adrs    = adrRR  asm
            (a, as) = Reg.take1Arg args
-- }}}
-- {{{ instance implementaion 
  instance StackMachine Assembler where
    load program = ASM (Mem.load program) Reg.empty Reg.empty
    readM    asm = Mem.read $ memory asm
    next     asm = ASM (Mem.move 1 mem) args adrs
        where mem  = memory asm
              args = argRR  asm
              adrs = adrRR  asm

    top   = Reg.top . argRR

    push  = simpleOp1Arg Reg.push
    read  = simpleOp1Arg Reg.read
    write = simpleOp1Arg Reg.write
    store = simpleOp1Arg Reg.store

    pop asm =  ASM mem (Reg.pop args) adrs
        where mem  = memory asm
              args = argRR  asm
              adrs = adrRR  asm


    neg = operate1Arg (-!) Reg.take1Arg 

    add = operate2Args (+)
    mul = operate2Args (*)
    sub = operate2Args (-)

    div = operate2Args (/!)

    eq  = operate2Args (==!)
    neq = operate2Args (/=!)
    lt  = operate2Args (<!)
    leq = operate2Args (<=!)
    gt  = operate2Args (>!)
    geq = operate2Args (>=!)

    jump n asm = ASM (Mem.move n mem) args adrs
      where mem     = memory asm
            args    = argRR  asm
            adrs    = adrRR  asm

    jnon0 = moveIf (0 /=) 
    jump0 = moveIf (0 ==)

    call retA n asm = jump n (ASM mem args (Reg.push retA adrs))
      where mem     = memory asm
            args    = argRR  asm
            adrs    = adrRR  asm
    
    ret curA asm = (adr, jump adr (ASM mem args as))
      where mem     = memory asm
            args    = argRR  asm
            adrs    = adrRR  asm
            (a, as) = Reg.take1Arg adrs
            adr     = a - curA
-- }}}
