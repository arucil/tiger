open Base

module Format = Caml.Format

type col_num = int [@@deriving show]

type line_num = int [@@deriving show]

type pos = line_num * col_num [@@deriving show]

let oc = ref Stdio.stderr

let source_name = ref "unknown"

let errors = ref false

let report (line, col) fmt =
  errors := true;
  Stdio.Out_channel.fprintf !oc "%s:%d:%d: error: " !source_name line col;
  Stdio.Out_channel.kfprintf
    (fun oc -> Stdio.Out_channel.newline oc)
    !oc fmt

let set_out oc' =
  oc := oc'

let out () = !oc

let set_source_name name =
  source_name := name

let has_errors () = !errors