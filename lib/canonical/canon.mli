open Semantic

val linearize : Ir.stmt -> Temp.temp_store -> Ir.stmt list

val basic_blocks : Ir.stmt list -> Temp.temp_store -> Ir.stmt list list * Temp.label

val trace_schedule : Ir.stmt list list -> Temp.label -> Temp.temp_store -> Ir.stmt list