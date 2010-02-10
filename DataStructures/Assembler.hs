module Assembler(Assembler) where
-- {{{ imports
  import Prelude hiding (div, read)
  import qualified Memory   as Mem
  import qualified Register as Reg
  import StackMachine
  import Operators
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
-- }}}
-- {{{ instance implementaion 
  instance StackMachine Assembler where
    load program = ASM (Mem.load program) Reg.empty Reg.empty

    push  = simpleOp1Arg Reg.push
    write = simpleOp1Arg Reg.write
    read  = simpleOp1Arg Reg.read

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
-- }}}
