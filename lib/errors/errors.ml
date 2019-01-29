open Base

let oc = ref Stdio.stderr

let report (line, col) fmt =
  Stdio.Out_channel.fprintf !oc "Error at (%d:%d): " line col;
  Stdio.Out_channel.kfprintf
    (fun oc -> Stdio.Out_channel.newline oc)
    !oc fmt

let set_out_channel oc' =
  oc := oc'