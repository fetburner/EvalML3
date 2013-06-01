{ 
open Parser
exception Lexing of string
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule token = parse
  | space+
      { token lexbuf }
  | "("
      { LPAREN }
  | ")"
      { RPAREN }
  | "="
      { EQUAL }
  | "<"
      { LESS }
  | "->"    
      { MINUS_GREATER } 
  | "+"
      { PLUS }
  | "-"
      { MINUS }
  | "*"
      { AST }
  | ";;"
      { EOI }
  | '-'? digit+
      { INT(int_of_string (Lexing.lexeme lexbuf)) }
  | "true"
      { BOOL(true) }
  | "false"
      { BOOL(false) }
  | "then"
      { THEN }
  | "else"
      { ELSE }
  | "let"
      { LET }
  | "fun"
      { FUN }
  | "rec"
      { REC }
  | "in"
      { IN }
  | "if"
      { IF }
  | "#quit"
      { QUIT }
  | lower (lower|upper|digit|'_')*
      { VARIABLE(Lexing.lexeme lexbuf) }
  | _
      { raise (Lexing (Lexing.lexeme lexbuf)) }
