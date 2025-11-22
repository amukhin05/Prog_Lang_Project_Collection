include Utils

let parse (s : string) : prog option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | e -> Some e
  | exception _ -> None

let desugar (p : prog) : expr =
  let rec fun_from_args args ty =
    match args with
    | [] -> ty
    | (_, t) :: rest -> FunTy (t, fun_from_args rest ty)
  in
  let rec curry_fun args e =
    match args with
    | [] -> e
    | (x, t) :: rest -> Fun (x, t, curry_fun rest e)
  in
  let rec desugar_sfexpr (sfe : sfexpr) : expr =
    match sfe with
    | SUnit -> Unit
    | SBool b -> Bool b
    | SNum n -> Num n
    | SVar x -> Var x
    | SFun { args; body } -> curry_fun args (desugar_sfexpr body)
    | SApp [] -> assert false
    | SApp (f :: args) ->
        List.fold_left (fun acc arg -> App (acc, arg)) (desugar_sfexpr f) (List.map desugar_sfexpr args)
    | SLet { is_rec; name; args; ty; binding; body } ->
        let binding' = curry_fun args (desugar_sfexpr binding) in
        let ty' = fun_from_args args ty in
        Let { is_rec; name; ty = ty'; binding = binding'; body = desugar_sfexpr body; }
    | SIf (e1, e2, e3) -> If (desugar_sfexpr e1, desugar_sfexpr e2, desugar_sfexpr e3)
    | SBop (op, e1, e2) -> Bop (op, desugar_sfexpr e1, desugar_sfexpr e2)
    | SAssert e -> Assert (desugar_sfexpr e)
  in
  let rec desugar_prog (toplets : prog) : expr =
    match toplets with
    | [] -> Unit
    | [ { is_rec; name; args; ty; binding } ] ->
        let binding' = curry_fun args (desugar_sfexpr binding) in
        let ty' = fun_from_args args ty in
        Let { is_rec; name; ty = ty'; binding = binding'; body = Var name; }
    | { is_rec; name; args; ty; binding } :: rest ->
        let binding' = curry_fun args (desugar_sfexpr binding) in
        let ty' = fun_from_args args ty in
        Let { is_rec; name; ty = ty'; binding = binding'; body = desugar_prog rest; }
  in desugar_prog p

let type_of (e : expr) : (ty, error) result =
  let ( let* ) r f = match r with Ok x -> f x | (Error _ as e) -> e in
  let lookup (gamma : (string * ty) list) (x : string) : (ty, error) result =
    match List.assoc_opt x gamma with
    | Some t -> Ok t
    | None -> Error (UnknownVar x)
  in
  let op_types (b : bop) : ty * ty =
    match b with
    | Add | Sub | Mul | Div | Mod -> (IntTy, IntTy)
    | Lt | Lte | Gt | Gte | Eq | Neq -> (IntTy, BoolTy)
    | And | Or -> (BoolTy, BoolTy)
  in
  let rec iter (gamma : (string * ty) list) (e : expr) : (ty, error) result =
    match e with
    | Unit -> Ok UnitTy
    | Bool _ -> Ok BoolTy
    | Num _ -> Ok IntTy
    | Var x -> lookup gamma x
    | Fun (x, ty, body) ->
      let* t_body = iter ((x, ty) :: gamma) body in
      Ok (FunTy (ty, t_body))
    | App (e1, e2) ->
      let* t_fun = iter gamma e1 in
      (match t_fun with
      | FunTy (t_exp, t_res) ->
        let* t_arg = iter gamma e2 in
        if t_arg = t_exp then Ok t_res
        else Error (FunArgTyErr (t_exp, t_arg))
      | t_err -> Error (FunAppTyErr t_err))
    | If (e1, e2, e3) ->
      let* t1 = iter gamma e1 in
      if t1 <> BoolTy then Error (IfCondTyErr t1)
      else
        let* t_then = iter gamma e2 in
        let* t_else = iter gamma e3 in
        if t_then = t_else then Ok t_then
        else Error (IfTyErr (t_then, t_else))
    | Bop (b, e1, e2) ->
      let t_exp, t_res = op_types b in
      let* t1 = iter gamma e1 in
      if t1 <> t_exp then Error (OpTyErrL (b, t_exp, t1))
      else
        let* t2 = iter gamma e2 in
        if t2 <> t_exp then Error (OpTyErrR (b, t_exp, t2))
        else Ok t_res
    | Let { is_rec = false; name; ty; binding; body } ->
      let* t_bind = iter gamma binding in
      if t_bind <> ty then Error (LetTyErr (ty, t_bind))
      else iter ((name, ty) :: gamma) body
    | Let { is_rec = true; name; ty; binding; body } ->
      (match binding with
      | Fun _ ->
        let gamma_rec = (name, ty) :: gamma in
        let* t_bind = iter gamma_rec binding in
        if t_bind <> ty then Error (LetTyErr (ty, t_bind))
        else iter gamma_rec body
      | _ -> Error (LetRecErr name))
    | Assert e1 ->
      let* t = iter gamma e1 in
      if t = BoolTy then Ok UnitTy
      else Error (AssertTyErr t)
  in iter [] e

exception AssertFail
exception DivByZero

let eval (e : expr) :  value =
  let rec eval_expr (env : dyn_env) (e : expr) : value =
    match e with
    | Unit -> VUnit
    | Bool b -> VBool b
    | Num n -> VNum n
    | Var x -> Env.find x env
    | Bop (bop, e1, e2) ->
      (match bop with
      | And ->
        (match eval_expr env e1 with
        | VBool false -> VBool false
        | VBool true ->
          (match eval_expr env e2 with
          | VBool b2 -> VBool b2
          | _ -> assert false)
        | _ -> assert false)
      | Or ->
        (match eval_expr env e1 with
        | VBool true -> VBool true
        | VBool false ->
          (match eval_expr env e2 with
          | VBool b2 -> VBool b2
          | _ -> assert false)
        | _ -> assert false)
      | Add ->
        (match eval_expr env e1, eval_expr env e2 with
        | VNum n1, VNum n2 -> VNum (n1 + n2)
        | _ -> assert false)
      | Sub ->
        (match eval_expr env e1, eval_expr env e2 with
        | VNum n1, VNum n2 -> VNum (n1 - n2)
        | _ -> assert false)
      | Mul ->
        (match eval_expr env e1, eval_expr env e2 with
        | VNum n1, VNum n2 -> VNum (n1 * n2)
        | _ -> assert false)
      | Div ->
        (match eval_expr env e1, eval_expr env e2 with
        | VNum _, VNum 0 -> raise DivByZero
        | VNum n1, VNum n2 -> VNum (n1 / n2)
        | _ -> assert false)
      | Mod ->
        (match eval_expr env e1, eval_expr env e2 with
        | VNum _, VNum 0 -> raise DivByZero
        | VNum n1, VNum n2 -> VNum (n1 mod n2)
        | _ -> assert false)
      | Lt ->
        (match eval_expr env e1, eval_expr env e2 with
        | VNum n1, VNum n2 -> VBool (n1 < n2)
        | _ -> assert false)
      | Lte ->
        (match eval_expr env e1, eval_expr env e2 with
        | VNum n1, VNum n2 -> VBool (n1 <= n2)
        | _ -> assert false)
      | Gt ->
        (match eval_expr env e1, eval_expr env e2 with
        | VNum n1, VNum n2 -> VBool (n1 > n2)
        | _ -> assert false)
      | Gte ->
        (match eval_expr env e1, eval_expr env e2 with
        | VNum n1, VNum n2 -> VBool (n1 >= n2)
        | _ -> assert false)
      | Eq ->
        (match eval_expr env e1, eval_expr env e2 with
        | VNum n1, VNum n2 -> VBool (n1 = n2)
        | _ -> assert false)
      | Neq ->
        (match eval_expr env e1, eval_expr env e2 with
        | VNum n1, VNum n2 -> VBool (n1 <> n2)
        | _ -> assert false))
    | If (e1, e2, e3) ->
      (match eval_expr env e1 with
      | VBool true -> eval_expr env e2
      | VBool false -> eval_expr env e3
      | _ -> assert false)
    | Fun (x, _, body) ->
      VClos { arg = x; body; env; name = None }
    | App (e1, e2) ->
      (match (eval_expr env e1) with
      | VClos { arg; body; env = clos_env; name } ->
        let self_env =
          match name with
          | None -> clos_env
          | Some f -> Env.add f (eval_expr env e1) clos_env
        in
        let env' = Env.add arg (eval_expr env e2) self_env in
        eval_expr env' body
      | _ -> assert false)
    | Let { is_rec = false; name; ty = _; binding; body } ->
      let v1 = eval_expr env binding in
      let env' = Env.add name v1 env in
      eval_expr env' body
    | Let { is_rec = true; name; ty = _; binding; body } ->
      let v1 = eval_expr env binding in
      (match v1 with
      | VClos { arg; body = fun_body; env = clos_env; _ } ->
        let v_rec = VClos { arg; body = fun_body; env = clos_env; name = Some name } in
        let env' = Env.add name v_rec env in
        eval_expr env' body
      | _ -> assert false)
    | Assert e1 ->
      (match eval_expr env e1 with
      | VBool true -> VUnit
      | _ -> raise AssertFail)
  in eval_expr Env.empty e

let interp (s : string) : (value, error) result =
  match parse s with
  | None -> Error ParseErr
  | Some prog ->
      let e = desugar prog in
      match type_of e with
      | Error err -> Error err
      | Ok _ -> Ok (eval e)
