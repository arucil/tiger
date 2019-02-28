open Base

(* find escaped variables and mark their 'escape' field true. don't report any semantic errors *)
val find_escape : Ast.expr -> unit