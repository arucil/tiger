
type col_num = int [@@deriving show]

type line_num = int [@@deriving show]

type token_pos = line_num * col_num [@@deriving show]

type token =
  | WHILE of token_pos
  | FOR of token_pos
  | TO of token_pos
  | BREAK of token_pos
  | LET of token_pos
  | IN of token_pos
  | END of token_pos
  | FUNCTION of token_pos
  | VAR of token_pos
  | TYPE of token_pos
  | ARRAY of token_pos
  | IF of token_pos
  | THEN of token_pos
  | ELSE of token_pos
  | DO of token_pos
  | OF of token_pos
  | NIL of token_pos
  | COMMA of token_pos
  | COLON of token_pos
  | SEMI of token_pos
  | LPAREN of token_pos
  | RPAREN of token_pos
  | LBRACK of token_pos
  | RBRACK of token_pos
  | LBRACE of token_pos
  | RBRACE of token_pos
  | DOT of token_pos
  | PLUS of token_pos
  | MINUS of token_pos
  | TIMES of token_pos
  | DIV of token_pos
  | EQ of token_pos
  | NEQ of token_pos
  | GT of token_pos
  | GE of token_pos
  | LT of token_pos
  | LE of token_pos
  | AND of token_pos
  | OR of token_pos
  | ASSIGN of token_pos
  | ID of (token_pos * string)
  | STR of (token_pos * string)
  | INT of (token_pos * int)
  | EOF of token_pos
  [@@deriving show]

