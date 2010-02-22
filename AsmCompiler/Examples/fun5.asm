main

  push 1.0
  push 2.0
  jump fun2

  fun1
    mul
    jump end

  fun2
    add
    push 3.0
    jump fun1

  fun3
    sub

end  
