type t =
  | Int of int
  | Bool of bool
  | Fun of env * string * Exp.t
  | Rec of env * string * string * Exp.t
and bind = string * t
and env = bind list

let rec to_string = function
  | Int (i) -> string_of_int i
  | Bool (b) -> string_of_bool b
  | Fun (env, x, e0) ->
      "(" ^ env_to_string env ^ ")[fun " ^ x ^ " -> " ^
        Exp.to_string e0 ^ "]"
  | Rec (env, x, y, e0) ->
      "(" ^ env_to_string env ^ ")[rec " ^ x ^ " = fun " ^
        y ^ " -> " ^ Exp.to_string e0 ^ "]"
(*
 * val bind_to_string : bind -> string
 * 束縛を文字列で表現する
 *)
and bind_to_string (x, v) = x ^ " = " ^ to_string v
and env_to_string = function
  | [] -> ""
  | b0 :: env ->
      List.fold_left (fun acc b -> bind_to_string b ^ ", " ^ acc)
        (bind_to_string b0) env
