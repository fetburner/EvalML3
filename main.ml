external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"

(* 
 * val read_eval_print_loop : unit -> unit
 * 導出システムEvalML3のREPL
 *)
let rec read_eval_print_loop () =
  print_string "# ";
  flush stdout;
  try
    match Parser.toplevel Lexer.token (Lexing.from_channel stdin) with
    | Toplevel.Exp (exp) ->
        Deriv.of_Exp exp
          |> Deriv.to_string
          |> print_endline;
        read_eval_print_loop ()
    | Toplevel.Quit -> ()
  with
  | Parsing.Parse_error | Lexer.Lexing _ ->
      print_endline "Error: Syntax error";
      read_eval_print_loop ()
  | Deriv.Eval (env, exp) ->
      print_endline ("Error: " ^ Value.env_to_string env ^ " |- " ^
        Exp.to_string exp ^ " を評価する規則がありません");
      read_eval_print_loop ()

let _ = read_eval_print_loop ()
