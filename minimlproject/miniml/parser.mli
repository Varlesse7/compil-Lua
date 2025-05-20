type token =
  | LET
  | IN
  | IF
  | THEN
  | ELSE
  | FUN
  | EQ
  | INT of (int)
  | IDENT of (string)
  | TRUE
  | FALSE
  | REC
  | EOF
  | LPAREN
  | RPAREN
  | COMMA
  | RIGHT_ARROW

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.expr
