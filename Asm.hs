module Main where
 -- {{{ imports
  import Prelude hiding (div, read)
  import qualified Structure.Instance.Instruction as Ins
  import System.Environment (getArgs)
-- }}}
-- {{{ I/O functions
  interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)
-- }}}
-- {{{ main
  main = mainWith myFunction
    where mainWith function = do
            args <- getArgs
            case args of
              [input, output] -> interactWith function input output
              _ -> error "exaclty two parameters needed"
          myFunction = compile 
-- }}}          
-- {{{ helper functions
  parseAll :: [String] -> ([Ins.Instruction String], [String])
  parseAll = Ins.parseAll

  compile input 
    | null errors = unlines (map show parsed)
    | otherwise   = unlines errors              
      where (parsed, errors) = parseAll (lines input)
-- }}}
