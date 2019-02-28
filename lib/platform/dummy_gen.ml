open Base
open Common
open Ir

let stmt_gen stmt temp_store =
  let instrs = ref [] in
  let emit instr =
    instrs := instr :: !instrs
  in
  let rec munch_stmt = function
    | Seq (s1, s2) -> (munch_stmt s1; munch_stmt s2)
    | Jump
  in
  munch_stmt stmt;
  List.rev !instrs

let gen stmts temp_store =
  List.map stmts ~f:(fun stmt -> stmt_gen stmt temp_store)
    |> List.concat