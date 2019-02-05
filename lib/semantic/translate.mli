
module type S = sig
  type level
  type access

  val outermost : level

  val new_level : level -> Temp.label -> bool list -> Temp.temp_store -> level

  val params : level -> access list

  val new_local : level -> bool -> Temp.temp_store -> access
end

module Make : functor (Frame : Frame.S) -> S