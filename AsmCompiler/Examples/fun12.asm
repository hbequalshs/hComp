push 2.0

fun1 
  jnon0 fun2
  push 1.0
  jump end

fun2
  push 0.0
  jump fun1
  jump end

end
