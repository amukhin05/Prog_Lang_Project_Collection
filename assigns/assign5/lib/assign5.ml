
let curry3 f = fun a b c -> f (a, b, c)

let uncurry3 f = fun (a, b, c) -> f a b c

let rec filter_map p l = 
  match l with
  | [] -> []
  | curr :: rest ->
      match p curr with
      | Some elem -> elem :: filter_map p rest
      | None -> filter_map p rest

type 'a rtree = Node of 'a * 'a rtree list

let rec map_rtree f t = 
  match t with
  | Node (value, children) -> Node (f value, List.map (map_rtree f) children)

let rec filter_rtree f t = 
  match t with
  | Node (value, children) -> 
      if f value then Some (Node (value, filter_map (filter_rtree f) children))
      else None

type op = Add | Sub | Mul | Div

type expr =
  | Int of int
  | Var of string
  | Call of string * expr list
  | Bop of op * expr * expr

type stmt =
  | FunDef of string * string list * stmt list
  | Assign of string * expr
  | Print of expr
  | Return of expr

type prog = stmt list

let string_of_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"

let rec string_of_expr e = 
  match e with
  | Int n -> string_of_int n
  | Var x -> x
  | Call (name, vars) -> 
      let rec vars_to_string = function
      | [] -> ""
      | [curr] -> string_of_expr curr
      | curr :: rest -> string_of_expr curr ^ ", " ^ vars_to_string rest
      in name ^ "(" ^ vars_to_string vars ^ ")"
  | Bop (op, e1, e2) -> "(" ^ string_of_expr e1 ^ " " ^ string_of_op op ^ " " ^ string_of_expr e2 ^ ")"

let string_of_stmt s = 
  let rec expr_string st indent = 
    match st with 
    | FunDef (name, vars, body) ->
        let header = string_of_expr (Call (name, List.map (fun v -> Var v) vars)) in
        let new_indent = indent ^ "    " in
        let rec body_to_string = function
        | [] -> ""
        | [curr] -> expr_string curr new_indent
        | curr :: rest -> expr_string curr new_indent ^ "\n" ^ body_to_string rest
        in indent ^ "def " ^ header ^ ":\n" ^ body_to_string body
    | Assign (var, body) -> indent ^ var ^ " = " ^ string_of_expr body
    | Print expr -> indent ^ "print(" ^ string_of_expr expr ^ ")"
    | Return expr -> indent ^ "return " ^ string_of_expr expr
  in expr_string s ""

let rec string_of_prog p = 
  match p with
  | [] -> ""
  | [curr] -> string_of_stmt curr
  | curr :: rest -> string_of_stmt curr ^ "\n" ^ string_of_prog rest
