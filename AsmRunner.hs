module Main where
 -- {{{ imports
  import Prelude hiding (div)
  import Structure.Instance.Assembler as Asm
  import Structure.Instance.Instruction as Ins
  import System.Environment (getArgs)
-- }}}
-- {{{ I/O functions
  interactWith function inputFile = do
    input <- readFile inputFile
    print (function input)
-- }}}
-- {{{ main
  main = mainWith myFunction
    where mainWith function = do
            args <- getArgs
            case args of
              [input] -> interactWith function input
              _ -> error "exaclty one parameter needed"
          myFunction = exec
-- }}}          
-- {{{ helper functions
  exec input = run 0 asm
    where asm = (Asm.load $ map (Ins.read :: String -> Instruction String) (lines input)) :: Assembler (Instruction String) Double
          run n asm =
            case instruction of
              Just (Ins code Nothing ) -> case code of
                                            0x01 -> run (n + 1)   $ next (pop asm)
                                            0x02 -> run (n + 1)   $ next (neg asm)
                                            0x03 -> run (n + 1)   $ next (add asm)
                                            0x04 -> run (n + 1)   $ next (mul asm)
                                            0x05 -> run (n + 1)   $ next (sub asm)
                                            0x06 -> run (n + 1)   $ next (div asm)
                                            0x07 -> run (n + 1)   $ next (eq asm)
                                            0x08 -> run (n + 1)   $ next (neq asm)
                                            0x09 -> run (n + 1)   $ next (lt asm)
                                            0x0a -> run (n + 1)   $ next (leq asm)
                                            0x0b -> run (n + 1)   $ next (gt asm)
                                            0x0c -> run (n + 1)   $ next (geq asm)
                                            0x0d -> run (n + a)   $ as where (a, as) = ret n asm
                                            _    -> error "executable file corrupted" 
              Just (Ins code (Just a)) -> case code of
                                            0x0f -> run (n + 1)   $ next (push arg asm)
                                            0x11 -> run (n + 1)   $ next (write arg asm)
                                            0x12 -> run (n + 1)   $ next (Asm.read arg asm)
                                            0x13 -> run (n + arg) $ jump  arg asm
                                            0x14 -> run (n + a)   $ as where (a, as) = jnon0 arg asm
                                            0x15 -> run (n + a)   $ as where (a, as) = jump0 arg asm
                                            0x16 -> run (n + arg) $ call (n + 1) arg asm
                                            0x17 -> run (n + 1)   $ next (store arg asm)
                                            _    -> error "executable file corrupted"
                                            where arg = Prelude.read a :: Double
              Nothing              ->  top asm 
              where instruction = readM asm 
-- }}}
