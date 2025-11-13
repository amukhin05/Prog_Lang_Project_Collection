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

let type_of (_ : expr) : (ty, error) result = assert false

exception AssertFail
exception DivByZero

let eval (_ : expr) :  value = assert false

let interp (_ : string) : (value, error) result = assert false
