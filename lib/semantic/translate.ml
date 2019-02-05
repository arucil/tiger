open Base

module type S = sig
  type level
  type access

  val outermost : level

  val new_level : level -> Temp.label -> bool list -> Temp.temp_store -> level

  val params : level -> access list

  val new_local : level -> bool -> Temp.temp_store -> access
end

(* TODO: Exercise 6.5 eliminate unnecessary static links *)

module Make (Frame : Frame.S) = struct

  type level =
    {
      parent : level option;
      frame : Frame.t;
    }

  and access = level * Frame.access

  let outermost =
    let temp_store = Temp.new_store () in
    {
      parent = None;
      frame = Frame.new_frame (Temp.named_label "main") [] temp_store
    }

  let new_level parent label params temp_store =
    {
      parent = Some parent;
      frame = Frame.new_frame label (true :: params) temp_store;
    }

  let params level =
    Frame.params level.frame
      |> List.tl_exn
      |> List.map ~f:(fun acc -> (level, acc))

  let new_local level escape temp_store =
    (level, Frame.new_local level.frame escape temp_store)

end