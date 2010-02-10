module Instruction where

  import Code
  
  data Instruction a = Ins {
                     op_code  :: Int
                   , argument :: Maybe a
                   }

  instance Code Instruction where
    parse line = 
      let p = words line 
      in 
        case (length p) of
          0 -> Ins 0x00 Nothing
          1 -> case head p of
                 "pop"   = Ins 0x01 Nothing
                 "neg"   = Ins 0x02 Nothing
                 "add"   = Ins 0x03 Nothing
                 "mul"   = Ins 0x04 Nothing
                 "sub"   = Ins 0x05 Nothing
                 "div"   = Ins 0x06 Nothing
                 "eq"    = Ins 0x07 Nothing
                 "neq"   = Ins 0x08 Nothing
                 "lt"    = Ins 0x09 Nothing
                 "leq"   = Ins 0x0a Nothing
                 "gt"    = Ins 0x0b Nothing
                 "geq"   = Ins 0x0c Nothing
                 "ret"   = Ins 0x0d Nothing
                 label   = Ins 0x0e Nothing
          2 -> case head p of
                 "push"  = Ins 0x0f $ Just (last p)
                 "write" = Ins 0x11 $ Just (last p)
                 "read"  = Ins 0x12 $ Just (last p)
                 "jump"  = Ins 0x13 $ Just (last p)
                 "jnon0" = Ins 0x14 $ Just (last p)
                 "jump0" = Ins 0x15 $ Just (last p)
                 "call"  = Ins 0x16 $ Just (last p)
                 inst    -> error $ "pasre: unknown instruction" ++ instr
          n -> error $ "parse: invalid number of arguments " ++ (show n)
