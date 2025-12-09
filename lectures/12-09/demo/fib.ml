let rec fib = fun n ->
  if n < 2
  then n
  else fib (n - 1) + fib (n - 2)
let _ = assert (fib 38 = 39088169)
