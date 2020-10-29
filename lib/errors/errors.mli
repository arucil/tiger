open Base

module Format = Caml.Format

type col_num = int [@@deriving show]

type line_num = int [@@deriving show]

type pos = line_num * col_num [@@deriving show]

val report : pos -> ('a, Stdio.Out_channel.t, unit) format -> 'a

val set_out : Stdio.Out_channel.t -> unit

val out : unit -> Stdio.Out_channel.t

val set_source_name : string -> unit

val has_errors : unit -> bool