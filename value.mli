(* 値を表す型 *)
type t =
  | Int of int
  | Bool of bool
  | Fun of env * string * Exp.t
  | Rec of env * string * string * Exp.t
(* 変数の束縛を表す型 *)
and bind = string * t
(* 名前付きの環境を表す型 *)
and env = bind list

(* 値を文字列で表現する *)
val to_string : t -> string
(* 環境を文字列で表現する *)
val env_to_string : env -> string
