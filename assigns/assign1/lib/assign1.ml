let num_factors (n : int) : int = 
  let rec count x div num = 
    if x = 1 then num
    else if x mod div = 0 then count (x/div) 2 (num + 1)
    else count x (div + 1) num
  in count n 2 0

  let perfect_power (i : int) (n : int) : bool = 
    if n = 1 then true
    else if i = 1 then true
    else if i = (-1) && n = (-1) then true
    else if i = 0 then false
    else if i < 0 && n <> (-1) then false
    else if n = 0 && i > 0 then true
    else if i < 0 && n = (-1) then
      if i mod 2 = 0 then false
      else true
    else if n < 0 then 
      if i mod 2 = 0 then false
      else if n = -1 then true
      else 
        let rec back base : bool = 
          if let rec int_pow int_base exp : int =
              if exp = 0 then 1
              else int_base * int_pow int_base (exp - 1)
          in int_pow base i = n then true
          else if base < n then false
          else back (base-1)
        in back (-2)
    else
      let rec front base : bool = 
        if let rec int_pow int_base exp : int =
          if exp = 0 then 1
          else int_base * int_pow int_base (exp - 1)
        in int_pow base i = n then true
        else if base > n then false
        else front (base+1)
      in front 2

let collatz (n : int) : int = 
  let rec coll num iter : int = 
    if num = 1 then iter
    else if num mod 2 = 0 then coll (num / 2) (iter + 1)
    else coll ((num * 3) + 1) (iter + 1)
  in coll n 0

let tst_records (i : int) : int =
  if i = 0 then 1
  else let rec count curr tot max : int = 
    if tot = i then (curr-1)
    else if (collatz curr) > max then count (curr + 1) (tot+1) (collatz curr)
    else count (curr + 1) tot max
  in count 1 0 0
