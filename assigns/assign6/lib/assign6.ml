
type expr =
  | True
  | False
  | Int of int
  | Var of string
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Lte of expr * expr
  | If of expr * expr * expr
  | Let of string * expr * expr

let is_lowercase (c : char) : bool =
  c >= 'a' && c <= 'z'

let is_valid_var : string -> bool= String.for_all is_lowercase

let is_space (c : char) : bool =
  match c with
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

type sexpr =
  | Atom of string
  | List of sexpr list

type token =
  | Lparen
  | Rparen
  | Atom of string

let tokens_of_string (s : string) : token list =
  let rec go (out : token list) (i : int) : token list =
    if i >= String.length s then List.rev out
    else if is_space s.[i] then go out (i + 1)
    else if s.[i] = '(' then go (Lparen :: out) (i + 1)
    else if s.[i] = ')' then go (Rparen :: out) (i + 1)
    else go' out i (i + 1)
  and go' (out : token list) (i : int) (j : int) : token list =
    if j >= String.length s || is_space s.[j] || s.[j] = '(' || s.[j] = ')'
      then go (Atom (String.sub s i (j - i)) :: out) j
    else go' out i (j + 1)
  in go [] 0

let rec sexpr_of_tokens (toks : token list) : (sexpr * token list) option =
  match toks with
  | Atom curr :: rest -> Some (Atom curr, rest)
  | Lparen :: rest -> 
      (match sexprs_of_tokens rest with
      | (last, Rparen :: rest') -> Some (List last, rest')
      | _ -> None)
  | _ -> None
and sexprs_of_tokens (toks : token list) : sexpr list * token list =
  let rec iter hold toks =
    match toks with
    | [] -> (List.rev hold, [])
    | Rparen :: _ -> (List.rev hold, toks)
    | _ ->
        (match sexpr_of_tokens toks with
        | None -> (List.rev hold, toks)
        | Some (e, rest) -> iter (e :: hold) rest)
  in iter [] toks

let parse_sexpr (s : string) : sexpr option =
  match sexpr_of_tokens (tokens_of_string s) with
  | Some (e, []) -> Some e
  | _ -> None

let rec expr_of_sexpr (sexpr : sexpr) : expr option = 
  let rec iter (lst : sexpr list) (res : expr list) : expr option =    
    match lst with
    | [] ->
        (match res with
        | [e] -> Some e
        | _ -> None)
    | curr :: rest -> 
        match curr with
        | Atom s ->
            if s = "" then None
            else if s = "True" then iter rest (True :: res)
            else if s = "False" then iter rest (False :: res)
            else
              (match int_of_string_opt s with
              | Some n -> iter rest (Int n :: res)
              | None ->
                  if is_valid_var s then iter rest (Var s :: res)
                  else None)
        | List (Atom "If" :: e1 :: e2 :: e3 :: rest) ->
            (match expr_of_sexpr e1, expr_of_sexpr e2, expr_of_sexpr e3 with
            | Some exp1, Some exp2, Some exp3 -> iter rest (If (exp1, exp2, exp3) :: res)
            | _ -> None)
        | List (Atom "Let" :: Atom x :: e1 :: e2 :: rest) when is_valid_var x ->
            (match expr_of_sexpr e1, expr_of_sexpr e2 with
            | Some exp1, Some exp2 -> iter rest (Let (x, exp1, exp2) :: res)
            | _ -> None)
        | List (Atom oper :: e1 :: e2 :: rest_ops) ->
            (match expr_of_sexpr e1, expr_of_sexpr e2 with
            | (Some exp1, Some exp2) ->
                (match oper with
                | "+"  -> iter rest_ops (Add (exp1, exp2) :: res)
                | "-"  -> iter rest_ops (Sub (exp1, exp2) :: res)
                | "*"  -> iter rest_ops (Mul (exp1, exp2) :: res)
                | "/"  -> iter rest_ops (Div (exp1, exp2) :: res)
                | "<=" -> iter rest_ops (Lte (exp1, exp2) :: res)
                | _ -> None)
            | _ -> None)
        | _ -> None
  in iter [sexpr] []

let parse (s : string) : expr option = 
  match parse_sexpr s with
  | None -> None
  | Some e -> expr_of_sexpr e

type ty =
  | IntTy
  | BoolTy

module M = Map.Make(String)
type ctxt = ty M.t

let empty_ctxt : ctxt = M.empty
let add_binding (x : string) (t : ty) (gamma : ctxt) : ctxt = M.add x t gamma
let check_binding (x : string) (gamma : ctxt) : ty option = M.find_opt x gamma

let rec type_of (gamma : ctxt) (e : expr) : ty option = 
  match e with
  | True -> Some BoolTy
  | False -> Some BoolTy
  | Int _ -> Some IntTy
  | Var x -> check_binding x gamma
  | Add (e1, e2) ->
      (match type_of gamma e1, type_of gamma e2 with
       | Some IntTy, Some IntTy -> Some IntTy
       | _ -> None)
  | Sub (e1, e2) ->
      (match type_of gamma e1, type_of gamma e2 with
       | Some IntTy, Some IntTy -> Some IntTy
       | _ -> None)
  | Mul (e1, e2) ->
      (match type_of gamma e1, type_of gamma e2 with
       | Some IntTy, Some IntTy -> Some IntTy
       | _ -> None)
  | Div (e1, e2) ->
      (match type_of gamma e1, type_of gamma e2 with
       | Some IntTy, Some IntTy -> Some IntTy
       | _ -> None)
  | Lte (e1, e2) ->
      (match type_of gamma e1, type_of gamma e2 with
       | Some IntTy, Some IntTy -> Some BoolTy
       | _ -> None)
  | If (e1, e2, e3) ->
      (match type_of gamma e1 with
       | Some BoolTy ->
           (match type_of gamma e2, type_of gamma e3 with
            | Some t1, Some t2 -> if t1 = t2 then Some t1 else None
            | _ -> None)
       | _ -> None)
  | Let (x, e1, e2) ->
      (match type_of gamma e1 with
       | Some t1 -> type_of (add_binding x t1 gamma) e2
       | None -> None)

let subst (e1 : expr) (x : string) (e2 : expr) : expr =
  let rec go e =
    match e with
    | True -> True
    | False -> False
    | Int n -> Int n
    | Var y -> if y = x then e1 else Var y
    | Add (e1, e2) -> Add (go e1, go e2)
    | Sub (e1, e2) -> Sub (go e1, go e2)
    | Mul (e1, e2) -> Mul (go e1, go e2)
    | Div (e1, e2) -> Div (go e1, go e2)
    | Lte (e1, e2) -> Lte (go e1, go e2)
    | If (e1, e2, e3) -> If (go e1, go e2, go e3)
    | Let (y, e1, e2) -> Let (y, go e1, if y = x then e2 else go e2)
  in go e2

type value =
  | IntV of int
  | BoolV of bool

let expr_of_value (v : value) : expr = 
  match v with
  | IntV n -> Int n
  | BoolV b -> if b then True else False

let rec eval (e : expr) : value = 
  match e with
  | True -> BoolV true
  | False -> BoolV false
  | Int n -> IntV n
  | Add (e1, e2) -> 
      (match eval e1, eval e2 with
      | IntV n1, IntV n2 -> IntV (n1 + n2)
      | _ -> assert false)
  | Sub (e1, e2) -> 
      (match eval e1, eval e2 with
      | IntV n1, IntV n2 -> IntV (n1 - n2)
      | _ -> assert false)
  | Mul (e1, e2) ->
      (match eval e1, eval e2 with
      | IntV n1, IntV n2 -> IntV (n1 * n2)
      | _ -> assert false)
  | Div (e1, e2) -> 
      (match eval e1, eval e2 with
      | IntV n1, IntV n2 -> IntV (n1 / n2)
      | _ -> assert false)
  | Lte (e1, e2) ->
      (match eval e1, eval e2 with
      | IntV n1, IntV n2 -> if n1 <= n2 then BoolV true else BoolV false
      | _ -> assert false)
  | If (e1, e2, e3) -> 
      (match eval e1 with
      | BoolV true ->
          (match eval e2 with
          | BoolV n1 -> BoolV n1
          | IntV n2 -> IntV n2)
      | BoolV false ->
          (match eval e3 with
          | BoolV n1 -> BoolV n1
          | IntV n2 -> IntV n2)
      | _ -> assert false)
  | Let (x, e1, e2) -> eval (subst (expr_of_value (eval e1)) x e2)
  | _ -> assert false

let interp ?(print=true) (s : string) : value option =
  let print s = if print then print_endline s else () in
  let _ = print "parsing..." in
  match parse s with
  | None -> None
  | Some e ->
    let _ = print "type_checking..." in
    match type_of empty_ctxt e with
    | None -> None
    | Some _ ->
      let _ = print "evaluating..." in
      Some (eval e)
