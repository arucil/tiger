open Base

module type S = sig

  type access

  val access_expr : access -> Ir.expr -> Ir.expr

  module Frame : sig
    type t

    val new_frame : Temp.label -> bool list -> Temp.temp_store -> t

    val label : t -> Temp.label

    val params : t -> access list

    val new_local : t -> bool -> Temp.temp_store -> access

  end

  val fp : Temp.temp

  val word_size : int

  val string_lit : Temp.label -> string -> unit

  val external_call : string -> Ir.expr list -> Ir.expr

end