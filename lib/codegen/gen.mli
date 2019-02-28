open Base
open Common

module type S = sig

  val stmt_gen : Ir.stmt -> Temp.temp_store -> Assem.t list

end

val gen : (module S) -> Ir.stmt list -> Temp.temp_store -> Assem.t list