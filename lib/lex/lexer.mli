
exception Error of Errors.pos * string


open Token

val get_token : Lexing.lexbuf -> token