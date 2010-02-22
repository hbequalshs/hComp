module Structure.Instance.Instruction
  ( Instruction(..)
  , parseAll
  , read) where

  import Structure.Class.Code

  data Instruction a = Ins {
                     op_code  :: Integer
                   , argument :: Maybe a
                   } deriving (Show, Read)

  getArgument :: Instruction String -> String
  getArgument ins =
    case argument ins of
      Just x  -> x
      Nothing -> ""

  find _ []     = Nothing
  find a (x:xs) 
    | a == fst x = Just (snd x)
    | otherwise  = find a xs

  instance Code Instruction where
    parse line = 
      let p = words line 
          return x = (Just x, Nothing) 
          error  x = (Nothing, Just x)
      in 
        case length p of
          0 -> return $ Ins 0x00 Nothing
          1 -> case head p of
                 "pop"   -> return $ Ins 0x01 Nothing
                 "neg"   -> return $ Ins 0x02 Nothing
                 "add"   -> return $ Ins 0x03 Nothing
                 "mul"   -> return $ Ins 0x04 Nothing
                 "sub"   -> return $ Ins 0x05 Nothing
                 "div"   -> return $ Ins 0x06 Nothing
                 "eq"    -> return $ Ins 0x07 Nothing
                 "neq"   -> return $ Ins 0x08 Nothing
                 "lt"    -> return $ Ins 0x09 Nothing
                 "leq"   -> return $ Ins 0x0a Nothing
                 "gt"    -> return $ Ins 0x0b Nothing
                 "geq"   -> return $ Ins 0x0c Nothing
                 "ret"   -> return $ Ins 0x0d Nothing
                 label   -> return $ Ins 0x0e $ Just label
          2 -> case head p of
                 "push"  -> return $ Ins 0x0f $ Just (last p)
                 "write" -> return $ Ins 0x11 $ Just (last p)
                 "read"  -> return $ Ins 0x12 $ Just (last p)
                 "jump"  -> return $ Ins 0x13 $ Just (last p)
                 "jnon0" -> return $ Ins 0x14 $ Just (last p)
                 "jump0" -> return $ Ins 0x15 $ Just (last p)
                 "call"  -> return $ Ins 0x16 $ Just (last p)
                 "store" -> return $ Ins 0x17 $ Just (last p)
                 instr   -> error $ "unknown instruction " ++ instr
          n -> error $ "invalid number of arguments " ++ (show n) 
   
    parseAll lines = reparse (parser 0 0 (lines) [] [] [])
      where parser _ _ []     parsed errors labels = (parsed, reverse errors, labels)
            parser n m (x:xs) parsed errors labels =
              case parse x of
                (Just pLine, Nothing) -> if code == 0x0e 
                                         then parser (n + 1) (m + 1) xs parsed errors ((getArgument pLine, n - m) : labels)
                                         else if code >= 0x13 && code <= 0x16
                                         then case find arg labels of
                                                Just k  -> parser (n + 1) m xs ((Ins code (Just (show (k - n + m)))) : parsed) errors labels 
                                                Nothing -> parser (n + 1) m xs ((Ins (code + 0x05) (Just (arg ++ " " ++ show(n - m)))) : parsed) errors labels
                                         else if code == 0x00
                                         then parser (n + 1) (m + 1) xs parsed errors labels
                                         else parser (n + 1) m xs (pLine : parsed) errors labels
                                         where code = op_code pLine
                                               arg  = getArgument pLine
                (Nothing, Just eLine) -> parser (n + 1) m xs parsed (("Line " ++ (show n) ++ ": " ++ eLine) : errors) labels
                _                     -> error "parseAll: unknown error"

            reparse (parsed, errors, labels) = reparser parsed [] []
              where reparser [] parsed errors2     = (parsed, (reverse errors2) ++ errors) 
                    reparser (x:xs) parsed errors2 =
                      if code > 0x17 
                      then case find (head args) labels of
                             Just k  -> reparser xs ((Ins (code - 0x05) (Just (show (k - (read (last args)::Integer))))) : parsed) errors2
                             Nothing -> reparser xs parsed (("Line " ++ (last args) ++ ": undefined function " ++ (head args)) : errors2)
                      else reparser xs (x : parsed) errors2 
                      where arg  = getArgument x
                            code = op_code x
                            args = words arg
