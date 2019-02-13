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

    val view_shift : t -> Ir.stmt -> Ir.stmt

  end

  val fp : Temp.temp

  val rv : Temp.temp

  val word_size : int

  type fragment

  val string_lit : Temp.label -> string -> fragment

  val fun' : Frame.t -> Ir.stmt -> fragment

  val external_call : string -> Ir.expr list -> Ir.expr

end