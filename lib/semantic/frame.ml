
module type S = sig
  type t
  type access

  val new_frame : Temp.label -> bool list -> Temp.temp_store -> t

  val label : t -> Temp.label

  val params : t -> access list

  val new_local : t -> bool -> Temp.temp_store -> access
end