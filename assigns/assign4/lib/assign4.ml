type token =
  | Lparen
  | Rparen
  | Atom of string

type sexpr =
  | Atom of string
  | List of sexpr list

type expr =
  | Int of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr

let is_space (c : char) : bool =
  match c with
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

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


(*FIRST IMPLEMENTED METHOD*)
let rec sexpr_of_tokens (toks : token list) : (sexpr * token list) option =
  match toks with
  | Atom curr :: rest -> Some (Atom curr, rest)
  | Lparen :: rest -> 
      (match sexprs_of_tokens rest with
      | (last, Rparen :: rest') -> Some (List last, rest')
      | _ -> None)
  | _ -> None
(*SECOND IMPLEMENTED METHOD*)
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


(*THIRD IMPLEMENTED METHOD*)
let parse_sexpr (s : string) : sexpr option =
  match sexpr_of_tokens (tokens_of_string s) with
  | Some (e, []) -> Some e
  | _ -> None

(*FOURTH IMPLEMENTED METHOD*)
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
            else 
                (try iter rest (Int (int_of_string s) :: res)
                with Failure _ -> None)
        | List (Atom oper :: first :: second :: rest) ->
            (match expr_of_sexpr first, expr_of_sexpr second with
            | (Some e1, Some e2) -> 
                (match oper with
                | "+" -> iter rest (Add (e1, e2) :: res)
                | "-" -> iter rest (Sub (e1, e2) :: res)
                | "*" -> iter rest (Mul (e1, e2) :: res)
                | "/" -> iter rest (Div (e1, e2) :: res)
                | _ -> None)
            | _ -> None)
        | _ -> None
  in iter [sexpr] []

  
(*FIFTH IMPLEMENTED METHODS (ALL 3)*)
let parse (s : string) : expr option =
  match parse_sexpr s with
  | None -> None
  | Some e -> expr_of_sexpr e

let rec eval (e : expr) : int =
  match e with
  | Int curr -> curr
  | Add (first, second) -> eval first + eval second
  | Sub (first, second) -> eval first - eval second
  | Mul (first, second) -> eval first * eval second
  | Div (first, second) -> eval first / eval second

let interp (s : string) : int option =
  match sexpr_of_tokens (tokens_of_string s) with
  | Some (sexp, []) -> (
      match expr_of_sexpr sexp with
      | Some e -> Some (eval e)
      | None -> None
    )
  | _ -> None
      
