(* 組み込み演算子を表す型 *)
type t =
  | Plus
  | Minus
  | Times
  | Lt

(* 組み込み演算子を文字列で表現する *)
let to_string = function
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Lt -> "<"
