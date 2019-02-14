open Parse.Ast

val trans_prog : (module Translate.S) -> expr -> Type.t * Translate.ir