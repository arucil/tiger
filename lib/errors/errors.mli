open Base
open Lex

val report : Token.token_pos -> ('a, Stdio.Out_channel.t, unit) format -> 'a

val set_out : Stdio.Out_channel.t -> unit

val out : unit -> Stdio.Out_channel.t