push 0.0

main
  jump0 fun1
  ret
  jump end

fun1
  push 1.0
  call main
  jump end

end  
