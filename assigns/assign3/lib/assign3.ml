let rec rev_append (l : 'a list) (r : 'a list) : 'a list =
  match l with
  | [] -> r
  | x :: xs -> rev_append xs (x :: r)

let is_whitespace c = List.mem c [' '; '\n'; '\t'; '\r']

let split_on_whitespace (s : string) : string list =
  let rec go acc i j =
    if i + j >= String.length s
    then List.rev (String.sub s i j :: acc)
    else
      if is_whitespace (String.get s (i + j))
      then go (String.sub s i j :: acc) (i + j + 1) 0
      else go acc i (j + 1)
  in go [] 0 0

let sep_on_whitespace (s : string) : string list =
  let rec go acc l =
    match l with
    | [] -> List.rev acc
    | h :: t ->
      if h = ""
      then go acc t
      else go (h :: acc) t
  in go [] (split_on_whitespace s)

type registers = (int * int) list

let load (l : int list) : registers = 
  let rec iter (arr : int list) (reg : registers) (idx : int) = 
    match arr with
    | [] -> List.rev reg
    | curr :: rest -> 
      if curr <> 0 then iter rest ((idx, curr) :: reg) (idx + 1) 
      else iter rest reg (idx + 1)
  in iter l [] 0 

let lookup (rs : registers) (i : int) : int = 
  let rec iter regs = 
    match regs with
    | [] -> 0
    | (r, v) :: rest ->
      if r = i then v
      else iter rest
  in iter rs

let update (rs : registers) (i : int) (amt: int) = 
  let rec iter regs = 
    match regs with
    | [] -> if amt = 0 then [] else [(i,amt)]
    | (k, v) :: rest -> 
        if k = i then 
          if amt = 0 then rest else (k, amt) :: rest
        else if k > i then 
          if amt = 0 then (k,v) :: rest else (i, amt) :: (k,v) :: rest
        else (k,v) :: iter rest
    in iter rs

let incr (rs : registers) (i : int) : registers = 
  update rs i ((lookup rs i) + 1)

let zero (rs : registers) (i : int) : registers = 
  update rs i 0

let transfer (rs : registers) (i : int) (j : int) : registers = 
  update rs j (lookup rs i)

let parse_urm (tokens : string list) : int list list =
  let rec iter acc toks =
    match toks with
    | [] -> List.rev acc
    | "Z" :: i :: rest -> iter ([0; int_of_string i] :: acc) rest
    | "I" :: i :: rest -> iter ([1; int_of_string i] :: acc) rest
    | "T" :: i :: j :: rest -> iter ([2; int_of_string i; int_of_string j] :: acc) rest
    | "J" :: i :: j :: k :: rest -> iter ([3; int_of_string i; int_of_string j; int_of_string k] :: acc) rest
    | _ -> []
  in iter [] tokens

let eval_urm (prog : int list list) (rs : registers) : registers =
  let rec iter regs pc =
    if pc >= (List.length prog) then regs
    else match (List.nth prog pc) with
    | [0; i] -> iter (zero regs i) (pc + 1)
    | [1; i] -> iter (incr regs i) (pc + 1)
    | [2; i; j] -> iter (transfer regs i j) (pc + 1)
    | [3; i; j; k] ->
        if lookup regs i = lookup regs j then iter regs k
        else iter regs (pc + 1)
    | _ -> []
  in iter rs 0

let interp_urm (prog : string) (args : int list) : int =
  prog
  |> sep_on_whitespace
  |> parse_urm
  |> fun prog -> eval_urm prog (load args)
  |> fun rs -> lookup rs 0

(* Challenge problem: make this work for negative inputs *)
let max_urm (i : int) (j : int) : int =
  interp_urm
    "
    T 0 2
    Z 0
    J 1 3 100
    I 0
    I 3
    J 0 0 2
    "
    [i; j]

let fibonacci_urm (i : int) : int =
  interp_urm
    "
    I 2
    J 0 5 11
      T 2 3
      J 1 4 7
        I 2 I 4
        J 0 0 3
      T 3 1
      Z 4 I 5
      J 0 0 1
    T 1 0
    "
    [i]
