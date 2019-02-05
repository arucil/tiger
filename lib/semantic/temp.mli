type temp_store

type label

type temp

val new_store : unit -> temp_store

val new_temp : temp_store -> temp

val new_label : temp_store -> label

val named_label : string -> label