open Base
open Common

module type S = sig

  val stmt_gen : Ir.stmt -> Temp.temp_store -> Assem.t list

end

let gen (module Gen : S) stmts temp_store =
  List.map stmts ~f:(fun stmt -> Gen.stmt_gen stmt temp_store)
    |> List.concat