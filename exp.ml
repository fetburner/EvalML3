type t =
  | Int of int
  | Bool of bool
  | Var of string
  | BinOp of t * Prim.t * t
  | If of t * t * t
  | Let of string * t * t
  | Fun of string * t
  | App of t * t
  | LetRec of string * string * t * t

let priority = function
  | Let _ | Fun _ | LetRec _ -> 0
  | If _ -> 10
  | BinOp (_, Prim.Lt, _) -> 20
  | BinOp (_, Prim.Plus, _) | BinOp (_, Prim.Minus, _) -> 30
  | BinOp (_, Prim.Times, _) -> 40
  | App _ -> 50
  | Int _ | Bool _ | Var _ -> 60

let rec to_string_aux prio exp = 
  (if prio > priority exp then fun str -> "(" ^ str ^ ")"
  else fun x -> x)
  (let to_string_assoc = to_string_aux (priority exp) in
  let to_string_nonassoc = to_string_aux (priority exp + 1) in
  let ( ^. ) x y = x ^ " " ^ y in
  match exp with
  | Int (i) -> string_of_int i
  | Bool (b) -> string_of_bool b
  | Var (x) -> x
  | BinOp (e1, op, e2) ->
      to_string_assoc e1 ^. Prim.to_string op ^. to_string_nonassoc e2
  | If (e1, e2, e3) ->
      "if" ^. to_string_assoc e1 ^. "then" ^. to_string_assoc e2 ^.
      "else" ^. to_string_assoc e3
  | Let (x, e1, e2) ->
      "let" ^. x ^. "=" ^. to_string_assoc e1 ^. "in" ^.
      to_string_assoc e2
  | Fun (x, e) ->
      "fun" ^. x ^. "->" ^. to_string_assoc e
  | App (e1, e2) ->
      to_string_assoc e1 ^. to_string_nonassoc e2
  | LetRec (x, y, e1, e2) ->
      "let rec" ^. x ^. "= fun" ^. y ^. "->" ^.
      to_string_assoc e1 ^. "in" ^. to_string_assoc e2)

let to_string = to_string_aux 0
