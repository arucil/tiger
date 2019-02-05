open Parse.Ast

val transProg : (module Frame.S) -> expr -> Type.t