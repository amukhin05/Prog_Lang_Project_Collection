open Utils

let parse (s : string) : expr option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | e -> Some e
  | exception _ -> None

let subst (v : value) (x : string) (e : expr) : expr =
  let val_as_expr = function
    | VNum n -> Num n
    | VBool true -> True
    | VBool false -> False
    | VUnit -> Unit
    | VFun (y, e0) -> Fun (y, e0)
  in
  let rec iter = function
    | Unit -> Unit 
    | True -> True
    | False -> False
    | Num n -> Num n
    | Var y -> if y = x then val_as_expr v else Var y
    | App (e1, e2) -> App (iter e1, iter e2)
    | Bop (b, e1, e2) -> Bop (b, iter e1, iter e2)
    | If (e1, e2, e3) -> If (iter e1, iter e2, iter e3)
    | Let (y, e1, e2) -> Let (y, iter e1, if y = x then e2 else iter e2)
    | Fun (y, body) -> if y = x then Fun (y, body) else Fun (y, iter body)
  in iter e

let eval (e : expr) : (value, error) result =
  (* let* really helped - credit to the piazza post on this *)
  let ( let* ) r f = match r with Ok x -> f x | (Error _ as e) -> e in
  let expect_int bop = function | VNum n -> Ok n | _ -> Error (InvalidArgs bop) in
  let expect_bool bop = function | VBool b -> Ok b | _ -> Error (InvalidArgs bop) in
  let rec iter = function
    | Unit -> Ok VUnit
    | True -> Ok (VBool true)
    | False -> Ok (VBool false)
    | Num n -> Ok (VNum n)
    | Var x -> Error (UnknownVar x)
    | If (e1, e2, e3) ->
        let* v1 = iter e1 in
        (match v1 with
         | VBool true  -> iter e2
         | VBool false -> iter e3
         | _ -> Error InvalidIfCond)
    | Let (x, e1, e2) ->
        let* v1 = iter e1 in
        iter (subst v1 x e2)
    | Fun (x, e) -> Ok (VFun (x, e))
    | App (e1, e2) ->
        let* v1 = iter e1 in
        (match v1 with
         | VFun (x, e) ->
             let* v2 = iter e2 in
             iter (subst v2 x e)
         | _ -> Error InvalidApp)
    | Bop (b, e1, e2) -> eval_bop b e1 e2
  and eval_bop b e1 e2 =
    match b with
    | And ->
        let* v1 = iter e1 in
        let* b1 = expect_bool And v1 in
        if not b1 then Ok (VBool false)
        else
          let* v2 = iter e2 in
          let* b2 = expect_bool And v2 in
          Ok (VBool b2)
    | Or ->
        let* v1 = iter e1 in
        let* b1 = expect_bool Or v1 in
        if b1 then Ok (VBool true)
        else
          let* v2 = iter e2 in
          let* b2 = expect_bool Or v2 in
          Ok (VBool b2)
    | _ -> 
      let* v1 = iter e1 in
      let* n1 = expect_int b v1 in
      let* v2 = iter e2 in
      let* n2 = expect_int b v2 in
      (match b with
        | Add -> Ok (VNum (n1 + n2))
        | Sub -> Ok (VNum (n1 - n2))
        | Mul -> Ok (VNum (n1 * n2))
        | Div -> if n2 = 0 then Error DivByZero else Ok (VNum (n1 / n2))
        | Mod -> if n2 = 0 then Error DivByZero else Ok (VNum (n1 mod n2))
        | Lt  -> Ok (VBool (n1 <  n2))
        | Lte -> Ok (VBool (n1 <= n2))
        | Gt  -> Ok (VBool (n1 >  n2))
        | Gte -> Ok (VBool (n1 >= n2))
        | Eq  -> Ok (VBool (n1 =  n2))
        | Neq -> Ok (VBool (n1 <> n2))
        | _ -> assert false)
  in iter e

let first_free_var (e : expr) : string option =
  let rec go (sl : string list) (e : expr) : string option =
    match e with
    | Unit -> None
    | True -> None
    | False -> None
    | Num _ -> None
    | Var x -> if List.mem x sl then None else Some x
    | App (e1, e2) ->
        (match go sl e1 with
         | Some v -> Some v
         | None -> go sl e2)
    | Bop (_, e1, e2) ->
        (match go sl e1 with
         | Some v -> Some v
         | None -> go sl e2)
    | If (e1, e2, e3) ->
        (match go sl e1 with
         | Some v -> Some v
         | None ->
             (match go sl e2 with
              | Some v -> Some v
              | None -> go sl e3))
    | Let (x, e1, e2) ->
        (match go sl e1 with
         | Some v -> Some v
         | None -> go (x :: sl) e2)
    | Fun (x, e) -> go (x :: sl) e
  in go [] e

let interp (s : string) : (value, error) result =
  match parse s with
  | None -> Error ParseFail
  | Some e ->
      (match first_free_var e with
       | Some v -> Error (UnknownVar v)
       | None -> eval e)
