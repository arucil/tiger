open Parse.Ast

val trans_prog : expr -> (module Translate.S) -> Temp.temp_store -> Type.t * Ir.stmt