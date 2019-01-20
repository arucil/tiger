
exception Error of Token.token_pos * string


val get_token : Lexing.lexbuf -> Token.t