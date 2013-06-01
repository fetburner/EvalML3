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

(* 式を文字列で表現する *)
val to_string : t -> string
