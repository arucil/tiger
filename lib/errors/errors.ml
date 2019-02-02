open Base

let oc = ref Stdio.stderr

let erroneous = ref false

let report (line, col) fmt =
  erroneous := true;
  Stdio.Out_channel.fprintf !oc "Error at (%d:%d): " line col;
  Stdio.Out_channel.kfprintf
    (fun oc -> Stdio.Out_channel.newline oc)
    !oc fmt

let set_out oc' =
  oc := oc'

let out () = !oc

let has_errors () = !erroneous