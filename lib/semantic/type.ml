open Base
open Common

type unique = int

type unique_store = int ref

let new_unique_store () = ref 0

let new_unique r =
  let i = !r in
  Int.incr r;
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
  | RecordType (fields, u) -> Printf.sprintf "record { %s }#%d"
      (List.map ~f:(fun (sym, ty) ->
                      Printf.sprintf "%s : %s" (Symbol.name sym) (show ty))
        fields
        |> String.concat ~sep:", ")
      u
  | ArrayType (ty, u) -> Printf.sprintf "(array of %s)#%d" (show ty) u

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
      | None -> Utils.Exn.unreachable ()
    end
  | ty -> ty

let (=) t1 t2 =
  let rec go uniques t1 t2 =
    match t1, t2 with
    | IntType, IntType -> true
    | StringType, StringType -> true
    | UnitType, UnitType -> true
    | NilType, NilType -> true
    | AliasType { name = name1; ty = ty1 }, AliasType { name = name2; ty = ty2 } ->
      Symbol.(name1 = name2) &&
      (match ty1, ty2 with
      | None, None -> true
      | Some t1, Some t2 -> go uniques t1 t2
      | _ -> false)
    | ArrayType (t1, u1), ArrayType (t2, u2) ->
      if u1 <> u2 then
        false
      else if Set.mem uniques u1 then
        true
      else
        let uniques = Set.add uniques u1 in
        go uniques t1 t2
    | RecordType (fields1, u1), RecordType (fields2, u2) ->
      if u1 <> u2 then
        false
      else if Set.mem uniques u1 then
        true
      else
        let uniques = Set.add uniques u1 in
        List.map2_exn fields1 fields2
          ~f:(fun (sym1, t1) (sym2, t2) -> Symbol.(sym1 = sym2) && go uniques t1 t2)
          |> List.for_all ~f:(fun t -> t)
    | _ -> false
  in
    go (Set.empty (module Int)) t1 t2

let (<>) t1 t2 = not (t1 = t2)

let is_compatible t1 t2 =
  t1 = t2
    || t1 = NilType && is_record t2
    || t2 = NilType && is_record t1

let rec eq_ignore_unique t1 t2 =
  match actual_type t1, actual_type t2 with
  | IntType, IntType -> true
  | StringType, StringType -> true
  | NilType, NilType -> true
  | UnitType, UnitType -> true
  | ArrayType (t1', _), ArrayType (t2', _) -> eq_ignore_unique t1' t2'
  | RecordType (fs1', _), RecordType (fs2', _) ->
    (match List.map2 fs1' fs2'
      ~f:(fun (name1, t1') (name2, t2') -> Poly.(name1 = name2)
            && eq_ignore_unique t1' t2')
    with
    | Unequal_lengths -> false
    | Ok res -> List.for_all res ~f:(fun _ -> true))
  | _ -> false