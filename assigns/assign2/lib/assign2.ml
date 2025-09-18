let drop_leading (n : int) (l : int list) : int list =
  let rec drop list =
    match list with
    | x :: rest when x = n -> drop rest
    | _ -> list
  in drop l

let drop_trailing (n : int) (l : int list) : int list =
  l |> List.rev |> drop_leading n |> List.rev

let split_on_char (c : char) (s : string) : string list =
  let rec iter (res : string list)  (pos : int) (prev : int) = 
    if pos = String.length s then
      List.rev (String.sub s prev (pos - prev) :: res)
    else if String.get s pos = c then
      iter (String.sub s prev (pos - prev) :: res) (pos + 1) (pos + 1)
    else iter res (pos + 1) prev
  in iter [] 0 0


let parse_fractran (input : string) : Q.t list =
  let parts = split_on_char ' ' input in
  let rec builder list = 
    match list with
    | first :: rest ->
      if first = "" then builder rest
      else 
        let split_frac = split_on_char '/' first in 
        Q.make (Z.of_string (List.nth split_frac 0)) (Z.of_string (List.nth split_frac 1)) :: builder rest
    | [] -> []
    in builder parts

let eval_fractran (program : Q.t list) (input : Z.t) : Z.t =
  let rec iter program n = 
    match program with
    | first :: rest -> 
      let r = Q.mul (Q.make n Z.one) first in
      if Q.den r = Z.one then iter program (Q.num r)
      else iter rest n
    | [] -> n
  in iter program input

let interp_fractran (input : string) : Z.t -> Z.t =
  eval_fractran (parse_fractran input)

let max_fractran (i : int) (j : int) : int =
  let program = "5/6 5/2 5/3" in
  let input = Z.((~$2 ** i) * (~$3 ** j)) in
  let output = interp_fractran program input in
  let div_by_five n =
    let rec go acc n =
      if n = Z.one
      then acc
      else go (acc + 1) Z.(n / ~$5)
    in go 0 n
  in div_by_five output

let fib_fractran (n : int) : int =
  let program = "17/65 133/34 17/19 23/17 2233/69 23/29 31/23 74/341 31/37 41/31 129/287 41/43 13/41 1/13 1/3" in
  let input = Z.(~$78 * (~$5 ** n)) in
  let output = interp_fractran program input in
  let div_by_two n =
    let rec go acc n =
      if n = Z.one
      then acc
      else go (acc + 1) Z.(n / ~$2)
    in go 0 n
  in div_by_two output
