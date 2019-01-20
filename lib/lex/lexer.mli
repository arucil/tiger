
exception Error of Token.token_pos * string


open Token

val get_token : Lexing.lexbuf -> token