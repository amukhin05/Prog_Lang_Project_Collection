include Utils

let parse (s : string) : prog option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | prog -> Some prog
  | exception _ -> None

let principle_type (ty : ty) (cs : constr list) : ty_scheme option =
  let rec lookup (a : ident) (s : (ident * ty) list) =
    match s with
    | [] -> None
    | (b, t) :: s' -> if a = b then Some t else lookup a s'
  in
  let rec apply (s : (ident * ty) list) (t : ty) : ty =
    match t with
    | TUnit | TInt | TFloat | TBool -> t
    | TVar a ->
        (match lookup a s with
         | None -> t
         | Some t' -> apply s t')
    | TList t1 -> TList (apply s t1)
    | TOption t1 -> TOption (apply s t1)
    | TPair (t1,t2) -> TPair (apply s t1, apply s t2)
    | TFun (t1,t2) -> TFun (apply s t1, apply s t2)
  in
  let rec exists (a : ident) (t : ty) : bool =
    match t with
    | TVar b -> a = b
    | TList t1
    | TOption t1 -> exists a t1
    | TPair (t1,t2)
    | TFun (t1,t2) -> exists a t1 || exists a t2
    | TUnit | TInt | TFloat | TBool -> false
  in
  let rec unify (s : (ident * ty) list) (cs : constr list) : (ident * ty) list option =
    match cs with
    | [] -> Some s
    | (t1, t2) :: rest ->
        let t1 = apply s t1 in
        let t2 = apply s t2 in
        match t1, t2 with
        | TUnit, TUnit | TInt, TInt | TFloat, TFloat | TBool, TBool -> unify s rest
        | TList a, TList b | TOption a, TOption b -> unify s ((a,b) :: rest)
        | TPair (a1,a2), TPair (b1,b2) | TFun (a1,a2),  TFun (b1,b2) 
          -> unify s ((a1,b1) :: (a2,b2) :: rest)
        | TVar a, t
        | t, TVar a -> 
          if t = TVar a then unify s rest
          else if exists a t then None
          else let s' = (a, t) :: s in unify s' rest
        | _ -> None
  in
  let rec free_var_ty (t : ty) (res : VarSet.t) : VarSet.t =
    match t with
    | TVar a -> VarSet.add a res
    | TList t1
    | TOption t1 -> free_var_ty t1 res
    | TPair (t1,t2)
    | TFun (t1,t2) -> free_var_ty t1 (free_var_ty t2 res)
    | TUnit | TInt | TFloat | TBool -> res
  in
  match unify [] cs with
  | None -> None
  | Some s ->
      let t' = apply s ty in
      let vars = free_var_ty t' VarSet.empty in
      Some (Forall (vars, t'))


let type_of (_ctxt: stc_env) (_e : expr) : ty_scheme option = assert false

let is_well_typed (_p : prog) : bool = assert false

exception AssertFail
exception DivByZero
exception CompareFunVals

let eval_expr (_env : dyn_env) (_e : expr) : value = assert false

let eval p =
  let rec nest = function
    | [] -> Unit
    | [{is_rec;name;binding}] -> Let {is_rec;name;binding;body = Var name}
    | {is_rec;name;binding} :: ls -> Let {is_rec;name;binding;body = nest ls}
  in eval_expr Env.empty (nest p)

let interp input =
  match parse input with
  | Some prog ->
    if is_well_typed prog
    then Ok (eval prog)
    else Error TypeError
  | None -> Error ParseError
