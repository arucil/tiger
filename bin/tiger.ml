(* This is a dummy executable *)

open Base
open Lex
open Parse

let src = {|
let
in
end
|}

let () =
  let expr = Parser.prog Lexer.get_token (Lexing.from_string src) in
  Stdio.print_endline (Ast.show_expr expr)