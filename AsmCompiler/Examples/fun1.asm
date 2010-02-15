fact
  read 0
  push 0.0
  jump0 rec
  
  pop
  push 1.0
  ret

rec
  read 0
  read 1
  push 1.0
  sub
  call fact

  mul
  store 1
  return
