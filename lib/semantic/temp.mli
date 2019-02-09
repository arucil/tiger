open Base
open Parse

type temp_store

type label = Symbol.t

type temp

val new_store : unit -> temp_store

val new_temp : temp_store -> temp

val new_label : temp_store -> label

val new_labels : int -> temp_store -> label list

val named_label : string -> label