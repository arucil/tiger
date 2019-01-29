open Parse.Ast

val transExpr : ?in_loop:bool -> Env.t -> expr -> Type.t