open Common

val trans_prog : Ast.expr -> (module Translate.S) -> Temp.temp_store -> Type.t * Ir.stmt