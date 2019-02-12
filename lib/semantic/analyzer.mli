open Parse.Ast

val trans_prog : (module Platform.S) -> expr -> Type.t * Translate.ir