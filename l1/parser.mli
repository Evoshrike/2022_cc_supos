type token =
  | INT of (int)
  | ADD
  | SUB
  | MUL
  | DIV
  | SEMICOLON
  | SKIP
  | GTEQ
  | ASSIGN
  | DEREF
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | WHILE
  | DO
  | VAR of (string)
  | LPAREN
  | RPAREN
  | BEGIN
  | END
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Past.expr
