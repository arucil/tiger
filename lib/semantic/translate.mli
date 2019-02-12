open Base
open Parse

type ir

module type S = sig
  type level
  type access

  val set_temp_store : Temp.temp_store -> unit

  val outermost : level

  val new_level : level -> Temp.label -> bool list -> level

  val params : level -> access list

  val new_local : level -> bool -> access

  val error : ir

  val simple_var : level -> access -> ir

  val field_var : record:ir -> Symbol.t -> (Symbol.t * Type.t) list -> ir

  val index_var : array:ir -> index:ir -> ir

  val int : int -> ir

  val str : string -> ir

  val nil : ir

  val unit : ir

  val unary : Ast.uop -> ir -> ir

  val binary : Ast.op -> ir -> ir -> ir

  val str_binary : Ast.op -> ir -> ir -> ir

  val if_stmt : cond:ir -> conseq:ir -> alt:ir -> ir

  val if' : cond:ir -> conseq:ir -> alt:ir -> ir

  val record : ir list -> ir

  val array : size:ir -> init:ir -> ir

  val break : Temp.label -> ir

  val while' : break:Temp.label -> cond:ir -> body:ir -> ir

  val for' : break:Temp.label -> access -> low:ir -> high:ir -> body:ir -> ir

end

module Make : functor (Platf : Platform.S) -> S