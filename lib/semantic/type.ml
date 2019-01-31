open Base
open Parse

type unique = int

let unique_counter = ref 0

let new_unique () =
  let i = !unique_counter in
  Int.incr unique_counter;
  i

type t =
  | IntType
  | StringType
  | RecordType of (Symbol.t * t) list * unique
  | ArrayType of t * unique
  | NilType
  | UnitType
  | AliasType of { name : Symbol.t; mutable ty : t option }

let rec show = function
  | IntType -> "int"
  | StringType -> "string"
  | UnitType -> "unit"
  | NilType -> "nil"
  | AliasType { name; _ } -> Printf.sprintf "alias '%s'" (Symbol.name name)
  | RecordType (fields, u) -> Printf.sprintf "#%d { %s }"
      u
      (List.map ~f:(fun (sym, ty) ->
                      Printf.sprintf "%s : %s" (Symbol.name sym) (show ty))
        fields
        |> String.concat ~sep:", ")
  | ArrayType (ty, u) -> Printf.sprintf "#%d (array of %s)" u (show ty)

let is_record = function
  | RecordType _ -> true
  | _ -> false

let is_array = function
  | ArrayType _ -> true
  | _ -> false

let rec actual_type = function
  | AliasType { ty; _ } ->
    begin
      match ty with
      | Some ty -> actual_type ty
      | None -> Utils.unreachable ()
    end
  | ty -> ty

let is_compatible t1 t2 =
  let open Poly in
  t1 = t2
    || t1 = NilType && is_record t2
    || t2 = NilType && is_record t1