
module type S = sig
  type t
  type access

  val fp : Temp.temp

  val word_size : int

  val new_frame : Temp.label -> bool list -> Temp.temp_store -> t

  val label : t -> Temp.label

  val params : t -> access list

  val new_local : t -> bool -> Temp.temp_store -> access

  val access_expr : access -> Ir.expr -> Ir.expr
end