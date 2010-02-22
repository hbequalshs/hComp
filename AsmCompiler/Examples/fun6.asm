main

  push 1.0
  push 1.0
  jump fun2
  jump end

  fun1
    push 3.0
    push 3.0
    push 3.0
    mul
    jump0 fun3
    jnon0 end

  fun2
    sub 
    jump0 fun1

  fun3
    sub
    jump end

end  
