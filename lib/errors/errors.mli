open Base
open Lex

val report : Token.token_pos -> ('a, Stdio.Out_channel.t, unit) format -> 'a

val set_out_channel : Stdio.Out_channel.t -> unit