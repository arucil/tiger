open Base

module Format = Caml.Format

type temp_store

type label = Symbol.t [@@deriving show]

type temp

val new_store : unit -> temp_store

val new_temp : temp_store -> temp

val temp_of_int : int -> temp

val new_label : temp_store -> label

val new_labels : int -> temp_store -> label list

val named_label : string -> label


val pp_temp : Format.formatter -> temp -> unit