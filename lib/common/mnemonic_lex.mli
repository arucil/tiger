open Base

type mpart =
  | Str of string
  | Src of int
  | Dest of int
  | Jump of int

val get_part : Lexing.lexbuf -> mpart option
