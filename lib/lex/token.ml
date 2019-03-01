
type token =
  | WHILE of Errors.pos
  | FOR of Errors.pos
  | TO of Errors.pos
  | BREAK of Errors.pos
  | LET of Errors.pos
  | IN of Errors.pos
  | END of Errors.pos
  | FUNCTION of Errors.pos
  | VAR of Errors.pos
  | TYPE of Errors.pos
  | ARRAY of Errors.pos
  | IF of Errors.pos
  | THEN of Errors.pos
  | ELSE of Errors.pos
  | DO of Errors.pos
  | OF of Errors.pos
  | NIL of Errors.pos
  | COMMA of Errors.pos
  | COLON of Errors.pos
  | SEMI of Errors.pos
  | LPAREN of Errors.pos
  | RPAREN of Errors.pos
  | LBRACK of Errors.pos
  | RBRACK of Errors.pos
  | LBRACE of Errors.pos
  | RBRACE of Errors.pos
  | DOT of Errors.pos
  | PLUS of Errors.pos
  | MINUS of Errors.pos
  | TIMES of Errors.pos
  | DIV of Errors.pos
  | EQ of Errors.pos
  | NEQ of Errors.pos
  | GT of Errors.pos
  | GE of Errors.pos
  | LT of Errors.pos
  | LE of Errors.pos
  | AND of Errors.pos
  | OR of Errors.pos
  | ASSIGN of Errors.pos
  | ID of (Errors.pos * string)
  | STR of (Errors.pos * string)
  | INT of (Errors.pos * int32)
  | EOF of Errors.pos
  [@@deriving show]

