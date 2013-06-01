exception Eval of Value.env * Exp.t

type t = {
  env : Value.env;
  exp : Exp.t;
  value : Value.t;
  rule : rule }
(* 導出規則 *)
and rule = 
  | EInt
  | EBool
  | EIfT of t * t
  | EIfF of t * t
  | EPlus of t * t
  | EMinus of t * t
  | ETimes of t * t
  | ELt of t * t
  | EVar1
  | EVar2 of t
  | ELet of t * t
  | EFun
  | EApp of t * t * t
  | ELetRec of t
  | EAppRec of t * t * t

(*
 * val to_string_aux : string -> ?tab:string -> t -> string
 * 指定された文字列でインデントしながら導出を文字列で表現する
 *)
let rec to_string_aux indent ?(tab = "  ") deriv = 
  let to_string_aux = to_string_aux (indent ^ tab) in
  indent ^ Value.env_to_string deriv.env ^ " |- " ^
    Exp.to_string deriv.exp ^ " evalto " ^
    Value.to_string deriv.value ^ " by " ^
    match deriv.rule with
    | EInt -> "E-Int {}"
    | EBool -> "E-Bool {}"
    | EIfT (d1, d2) ->
        "E-IfT {\n" ^
        to_string_aux d1 ^ ";\n" ^
        to_string_aux d2 ^ "\n" ^
        indent ^ "}"
    | EIfF (d1, d2) ->
        "E-IfF {\n" ^
        to_string_aux d1 ^ ";\n" ^
        to_string_aux d2 ^ "\n" ^
        indent ^ "}"
    | EPlus (d1, d2) ->
        "E-Plus {\n" ^
        to_string_aux d1 ^ ";\n" ^
        to_string_aux d2 ^ ";\n" ^
        indent ^ tab ^ Value.to_string d1.value ^
          " plus " ^ Value.to_string d2.value ^ " is " ^
          Value.to_string deriv.value ^ " by B-Plus {}\n" ^
        indent ^ "}"  
    | EMinus (d1, d2) ->
        "E-Minus {\n" ^
        to_string_aux d1 ^ ";\n" ^
        to_string_aux d2 ^ ";\n" ^
        indent ^ tab ^ Value.to_string d1.value ^
          " minus " ^ Value.to_string d2.value ^ " is " ^
          Value.to_string deriv.value ^ " by B-Minus {}\n" ^
        indent ^ "}"
    | ETimes (d1, d2) ->
        "E-Times {\n" ^
        to_string_aux d1 ^ ";\n" ^
        to_string_aux d2 ^ ";\n" ^
        indent ^ tab ^ Value.to_string d1.value ^
          " times " ^ Value.to_string d2.value ^ " is " ^
          Value.to_string deriv.value ^ " by B-Times {}\n" ^
        indent ^ "}"
    | ELt (d1, d2) ->
        "E-Lt {\n" ^
        to_string_aux d1 ^ ";\n" ^
        to_string_aux d2 ^ ";\n" ^
        indent ^ tab ^ Value.to_string d1.value ^
          " less than " ^ Value.to_string d2.value ^ " is " ^
          Value.to_string deriv.value ^ " by B-Lt {}\n" ^
        indent ^ "}"
    | EVar1 -> "E-Var1 {}"
    | EVar2 (d1) -> 
        "E-Var2 {\n" ^
        to_string_aux d1 ^ "\n" ^
        indent ^ "}"
    | ELet (d1, d2) ->
        "E-Let {\n" ^
        to_string_aux d1 ^ ";\n" ^
        to_string_aux d2 ^ "\n" ^
        indent ^ "}"
    | EFun -> "E-Fun {}"
    | EApp (d1, d2, d3) ->
        "E-App {\n" ^
        to_string_aux d1 ^ ";\n" ^
        to_string_aux d2 ^ ";\n" ^
        to_string_aux d3 ^ "\n" ^
        indent ^ "}"
    | ELetRec (d1) ->
        "E-LetRec {\n" ^
        to_string_aux d1 ^ "\n" ^
        indent ^ "}"
    | EAppRec (d1, d2, d3) ->
        "E-AppRec {\n" ^
        to_string_aux d1 ^ ";\n" ^
        to_string_aux d2 ^ ";\n" ^
        to_string_aux d3 ^ "\n" ^
        indent ^ "}"

let to_string = to_string_aux ""

(*
 * 与えられた式を評価しながら導出を残す
 *)
let rec deriv ?(env = []) exp =
  match exp with
  | Exp.Int (i) -> { env; exp; value = Value.Int (i); rule = EInt }
  | Exp.Bool (b) -> { env; exp; value = Value.Bool (b); rule = EBool }
  | Exp.Var (x) ->
      begin match env with
      | [] -> raise (Eval (env, exp))
      | (y, value) :: env' ->
          if x = y then { env; exp; value; rule = EVar1 }
          else
            let d1 = deriv ~env:env' exp in
            { env; exp; value = d1.value; rule = EVar2 (d1) }
      end
  | Exp.BinOp (e1, op, e2) ->
      let d1 = deriv ~env e1 in
      let d2 = deriv ~env e2 in
      begin match d1.value, op, d2.value with
      | Value.Int (i1), Prim.Plus, Value.Int (i2) ->
          { env; exp; value = Value.Int (i1 + i2); rule = EPlus (d1, d2) }
      | Value.Int (i1), Prim.Minus, Value.Int (i2) ->
          { env; exp; value = Value.Int (i1 - i2); rule = EMinus (d1, d2) }
      | Value.Int (i1), Prim.Times, Value.Int (i2) ->
          { env; exp; value = Value.Int (i1 * i2); rule = ETimes (d1, d2) }
      | Value.Int (i1), Prim.Lt, Value.Int (i2) ->
          { env; exp; value = Value.Bool (i1 < i2); rule = ELt (d1, d2) }
      | _ ->
          raise (Eval (env, exp))
      end
  | Exp.If (e1, e2, e3) ->
      let d1 = deriv ~env e1 in
      begin match d1.value with
      | Value.Bool (true) ->
          let d2 = deriv ~env e2 in
          { env; exp; value = d2.value; rule = EIfT (d1, d2) }
      | Value.Bool (false) ->
          let d3 = deriv ~env e3 in
          { env; exp; value = d3.value; rule = EIfF (d1, d3) }
      | _ ->
          raise (Eval (env, exp))
      end
  | Exp.Let (x, e1, e2) ->
      let d1 = deriv ~env e1 in
      let d2 = deriv ~env:((x, d1.value) :: env) e2 in
      { env; exp; value = d2.value; rule = ELet (d1, d2) }
  | Exp.Fun (x, e) ->
      { env; exp; value = Value.Fun (env, x, e); rule = EFun }
  | Exp.App (e1, e2) ->
      let d1 = deriv ~env e1 in
      let d2 = deriv ~env e2 in
      begin match d1.value with
      | Value.Fun (env2, x, e0) ->
          let d3 = deriv ~env:((x, d2.value) :: env2) e0 in
          { env; exp; value = d3.value; rule = EApp (d1, d2, d3) }
      | Value.Rec (env2, x, y, e0) ->
          let d3 = deriv ~env:((y, d2.value) :: (x, d1.value) :: env2) e0 in
          { env; exp; value = d3.value; rule = EAppRec (d1, d2, d3) }
      | _ ->
          raise (Eval (env, exp))
      end
  | Exp.LetRec (x, y, e1, e2) ->
      let d1 = deriv ~env:((x, Value.Rec (env, x, y, e1)) :: env) e2 in
      { env; exp; value = d1.value; rule = ELetRec (d1) }

let of_Exp = deriv
