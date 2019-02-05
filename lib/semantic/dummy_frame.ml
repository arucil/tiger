open Base

type t =
  {
    label : Temp.label;
    params : access list;
    (*
    TODO:
    instructions required to 'view shift'
    number of locals
    *)
  }

and access =
  | InFrame of int
  | InReg of Temp.temp

let new_frame label params temp_store =
  {
    label;
    params = List.map params
      ~f:(fun escape ->
        if escape then InFrame 0
        else InReg (Temp.new_temp temp_store))
  }

let label t = t.label

let params t = t.params

let new_local t escape temp_store =
  if escape then InFrame 0
  else InReg (Temp.new_temp temp_store)