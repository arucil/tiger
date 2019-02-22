open Semantic

val linearize : Ir.stmt -> Temp.temp_store -> Ir.stmt list

val basic_blocks : Ir.stmt list list -> Temp.temp_store -> Temp.label