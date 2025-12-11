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
      
let type_of (ctxt : stc_env) (e : expr) : ty_scheme option =
  let fresh () = TVar (gensym ()) in
  let rec lookup (a : ident) (s : (ident * ty) list) =
    match s with
    | [] -> None
    | (b, ty) :: rest -> if a = b then Some ty else lookup a rest
  in
  let rec apply (s : (ident * ty) list) (ty : ty) : ty =
    match ty with
    | TUnit | TInt | TFloat | TBool -> ty
    | TVar a ->
        (match lookup a s with
         | None -> ty
         | Some t -> apply s t)
    | TList t1 -> TList (apply s t1)
    | TOption t1 -> TOption (apply s t1)
    | TPair (t1, t2) -> TPair (apply s t1, apply s t2)
    | TFun (t1, t2) -> TFun (apply s t1, apply s t2)
  in
  let inst (Forall (vars, ty) : ty_scheme) : ty =
    apply (VarSet.fold (fun a s -> (a, TVar (gensym ())) :: s) vars []) ty
  in
  let lookup_stc (x : ident) (gamma : stc_env) : ty_scheme option = Env.find_opt x gamma in
  let rec infer (gamma : stc_env) (e : expr) : (ty * constr list) option =
    match e with
    | Unit -> Some (TUnit, [])
    | Bool _ -> Some (TBool, [])
    | Int _ -> Some (TInt, [])
    | Float _ -> Some (TFloat, [])
    | Nil -> Some (TList (fresh ()), [])
    | ENone -> Some (TOption (fresh ()), [])
    | Var x ->
        (match lookup_stc x gamma with
         | None -> None
         | Some s -> Some ((inst s), []))
    | ESome e ->
        (match infer gamma e with
         | None -> None
         | Some (t, c) -> Some (TOption t, c))
    | Assert (Bool false) -> Some ((fresh ()), [])
    | Assert e ->
        (match infer gamma e with
         | None -> None
         | Some (t, c) -> Some (TUnit, (t, TBool) :: c))
    | Annot (e, t_ann) ->
        (match infer gamma e with
         | None -> None
         | Some (t, c) -> Some (t_ann, (t, t_ann) :: c))
    | If (e1, e2, e3) ->
        (match infer gamma e1, infer gamma e2, infer gamma e3 with
         | Some (t1, c1), Some (t2, c2), Some (t3, c3) ->
             Some (t2, ((t1, TBool) :: (t2, t3) :: (c1 @ c2 @ c3)))
         | _ -> None)
    | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } ->
        let a = fresh () in
        let list_a = TList a in
        (match infer gamma matched with
         | None -> None
         | Some (t, c0) ->
             let gamma_cons =
               Env.add hd_name (Forall (VarSet.empty, a))
                 (Env.add tl_name (Forall (VarSet.empty, list_a)) gamma)
             in
             (match infer gamma_cons cons_case, infer gamma nil_case with
              | Some (t1, c1), Some (t2, c2) ->
                  Some (t1, ((t, list_a) :: (t1, t2) :: (c0 @ c1 @ c2)))
              | _ -> None))
    | OptMatch { matched; some_name; some_case; none_case } ->
        let a = fresh () in
        (match infer gamma matched with
         | None -> None
         | Some (t, c0) ->
             let gamma_some = Env.add some_name (Forall (VarSet.empty, a)) gamma in
             (match infer gamma_some some_case, infer gamma none_case with
              | Some (t1, c1), Some (t2, c2) ->
                  Some (t1, ((t, TOption a) :: (t1, t2) :: (c0 @ c1 @ c2)))
              | _ -> None))
    | PairMatch { matched; fst_name; snd_name; case } ->
        let a = fresh () in
        let b = fresh () in
        (match infer gamma matched with
         | None -> None
         | Some (t, c0) ->
             let gamma_pair =
               Env.add fst_name (Forall (VarSet.empty, a))
                 (Env.add snd_name (Forall (VarSet.empty, b)) gamma)
             in
             (match infer gamma_pair case with
              | None -> None
              | Some (t1, c1) -> Some (t1, ((t, TPair (a, b)) :: (c0 @ c1)))))
    | Fun (x, ty_opt, body) ->
        let arg_ty =
          match ty_opt with
          | None -> fresh ()
          | Some t -> t
        in
        let gamma1 = Env.add x (Forall (VarSet.empty, arg_ty)) gamma in
        (match infer gamma1 body with
         | None -> None
         | Some (t_body, c_body) -> Some (TFun (arg_ty, t_body), c_body))
    | App (e1, e2) ->
        let a = fresh () in
        (match infer gamma e1, infer gamma e2 with
         | Some (t1, c1), Some (t2, c2) -> Some (a, ((t1, TFun (t2, a)) :: (c1 @ c2)))
         | _ -> None)
    | Let { is_rec = false; name; binding; body } ->
        (match infer gamma binding with
         | None -> None
         | Some (t1, c1) ->
             (match principle_type t1 c1 with
              | None -> None
              | Some s ->
                  let gamma1 = Env.add name s gamma in
                  (match infer gamma1 body with
                   | None -> None
                   | Some (t2, c2) -> Some (t2, c1 @ c2))))
    | Let { is_rec = true; name; binding; body } ->
        let a = fresh () in
        let gamma1 = Env.add name (Forall (VarSet.empty, a)) gamma in
        (match infer gamma1 binding with
         | None -> None
         | Some (t1, c1) ->
             match principle_type t1 c1 with
             | None -> None
             | Some sigma_f ->
                 let gamma2 = Env.add name sigma_f gamma in
                 (match infer gamma2 body with
                  | None -> None
                  | Some (t2, c2) -> Some (t2, c1 @ c2)))
    | Bop (op, e1, e2) ->
        (match infer gamma e1, infer gamma e2 with
         | Some (t1, c1), Some (t2, c2) ->
            let t_res, extra =
               match op with
               | Add | Sub | Mul | Div | Mod -> TInt, [ (t1, TInt); (t2, TInt) ]
               | AddF | SubF | MulF | DivF | PowF -> TFloat, [ (t1, TFloat); (t2, TFloat) ]
               | And | Or -> TBool, [ (t1, TBool); (t2, TBool) ]
               | Lt | Lte | Gt | Gte | Eq | Neq -> TBool, [ (t1, t2) ]
               | Cons ->
                   let a = fresh () in
                   TList a, [ (t1, a); (t2, TList a) ]
               | Comma -> TPair (t1, t2), []
            in Some (t_res, extra @ c1 @ c2)
         | _ -> None)
  in
  match infer ctxt e with
  | None -> None
  | Some (t, cs) -> principle_type t cs

let is_well_typed (p : prog) : bool =
  let rec typed = function
    | [] -> Unit
    | [{ is_rec; name; binding }] -> Let { is_rec; name; binding; body = Var name }
    | { is_rec; name; binding } :: ls -> Let { is_rec; name; binding; body = typed ls }
  in match type_of Env.empty (typed p) with | None -> false | Some _ -> true

exception AssertFail
exception DivByZero
exception CompareFunVals

let rec eval_expr (env : dyn_env) (e : expr) : value =
  match e with
  | Unit -> VUnit
  | Bool b -> VBool b
  | Nil -> VList []
  | ENone -> VNone
  | Int n -> VInt n
  | Float f -> VFloat f
  | Var x -> Env.find x env
  | Assert e1 ->
      (match eval_expr env e1 with
       | VBool true -> VUnit
       | _ -> raise AssertFail)
  | ESome e1 -> VSome (eval_expr env e1)
  | Annot (e1, _) -> eval_expr env e1
  | If (e1, e2, e3) ->
      (match eval_expr env e1 with
       | VBool true -> eval_expr env e2
       | _ -> eval_expr env e3)
  | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } ->
      (match eval_expr env matched with
       | VList (v_hd :: v_tl) ->
           eval_expr (Env.add hd_name v_hd (Env.add tl_name (VList v_tl) env)) cons_case
       | _ -> eval_expr env nil_case)
  | OptMatch { matched; some_name; some_case; none_case } ->
      (match eval_expr env matched with
       | VSome v -> eval_expr (Env.add some_name v env) some_case
       | _ -> eval_expr env none_case)
  | PairMatch { matched; fst_name; snd_name; case } ->
      (match eval_expr env matched with
       | VPair (v1, v2) ->
           eval_expr (Env.add fst_name v1 (Env.add snd_name v2 env)) case
       | _ -> assert false)
  | Fun (x, _, body) -> VClos { name = None; arg = x; body; env }
  | App (e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match v1 with
       | VClos { name; arg; body; env = clos_env } ->
           let env2 =
             match name with
             | None -> Env.add arg v2 clos_env
             | Some f -> Env.add f v1 (Env.add arg v2 clos_env)
           in eval_expr env2 body
       | _ -> assert false)
  | Let { is_rec = false; name; binding; body } ->
      eval_expr (Env.add name (eval_expr env binding) env) body
  | Let { is_rec = true; name; binding; body } ->
      (match binding with
       | Fun (arg, _, fun_body) ->
           eval_expr (Env.add name (
            VClos { name = Some name; arg; body = fun_body; env }) env) body
       | _ -> assert false)
  | Bop (op, e1, e2) ->
      let v1 () = eval_expr env e1 in
      let v2 () = eval_expr env e2 in
      (match op with
       | Add ->
           (match v1 (), v2 () with
            | VInt n1, VInt n2 -> VInt (n1 + n2)
            | _ -> assert false)
       | Sub ->
           (match v1 (), v2 () with
            | VInt n1, VInt n2 -> VInt (n1 - n2)
            | _ -> assert false)
       | Mul ->
           (match v1 (), v2 () with
            | VInt n1, VInt n2 -> VInt (n1 * n2)
            | _ -> assert false)
       | Div ->
           (match v1 (), v2 () with
            | VInt _, VInt 0 -> raise DivByZero
            | VInt n1, VInt n2 -> VInt (n1 / n2)
            | _ -> assert false)
       | Mod ->
           (match v1 (), v2 () with
            | VInt _, VInt 0 -> raise DivByZero
            | VInt n1, VInt n2 -> VInt (n1 mod n2)
            | _ -> assert false)
       | AddF ->
           (match v1 (), v2 () with
            | VFloat x1, VFloat x2 -> VFloat (x1 +. x2)
            | _ -> assert false)
       | SubF ->
           (match v1 (), v2 () with
            | VFloat x1, VFloat x2 -> VFloat (x1 -. x2)
            | _ -> assert false)
       | MulF ->
           (match v1 (), v2 () with
            | VFloat x1, VFloat x2 -> VFloat (x1 *. x2)
            | _ -> assert false)
       | DivF ->
           (match v1 (), v2 () with
            | VFloat x1, VFloat x2 -> VFloat (x1 /. x2)
            | _ -> assert false)      
       | PowF ->
           (match v1 (), v2 () with
            | VFloat x1, VFloat x2 -> VFloat (x1 ** x2)
            | _ -> assert false)
       | And ->
           (match v1 () with
            | VBool false -> VBool false
            | VBool true ->
                (match v2 () with
                 | VBool b -> VBool b
                 | _ -> assert false)
            | _ -> assert false)
       | Or ->
           (match v1 () with
            | VBool true -> VBool true
            | VBool false ->
                (match v2 () with
                 | VBool b -> VBool b
                 | _ -> assert false)
            | _ -> assert false)
       | Lt ->
           (match (v1 ()), (v2 ()) with
            | VClos _, _ | _, VClos _ -> raise CompareFunVals
            | _ -> VBool ((v1 ()) < (v2 ())))
       | Lte ->
           (match (v1 ()), (v2 ()) with
            | VClos _, _ | _, VClos _ -> raise CompareFunVals
            | _ -> VBool ((v1 ()) <= (v2 ())))
       | Gt ->
           (match (v1 ()), (v2 ()) with
            | VClos _, _ | _, VClos _ -> raise CompareFunVals
            | _ -> VBool ((v1 ()) > (v2 ())))
       | Gte ->
           (match (v1 ()), (v2 ()) with
            | VClos _, _ | _, VClos _ -> raise CompareFunVals
            | _ -> VBool ((v1 ()) >= (v2 ())))
       | Eq ->
           (match (v1 ()), (v2 ()) with
            | VClos _, _ | _, VClos _ -> raise CompareFunVals
            | _ -> VBool ((v1 ()) = (v2 ())))
       | Neq ->
           (match (v1 ()), (v2 ()) with
            | VClos _, _ | _, VClos _ -> raise CompareFunVals
            | _ -> VBool ((v1 ()) <> (v2 ())))
       | Cons ->
           (match (v1 ()), (v2 ()) with
            | v, VList vs -> VList (v :: vs)
            | _ -> assert false)
       | Comma -> VPair ((v1 ()), (v2 ())))

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
