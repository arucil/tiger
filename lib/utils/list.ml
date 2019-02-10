
let rec iter2 xs ys ~f =
  match xs, ys with
  | (x :: xs'), (y :: ys') ->
    f x y;
    iter2 xs' ys' ~f
  | _ -> ()

let iter2i xs ys ~f =
  let rec go i xs ys =
    match xs, ys with
    | (x :: xs'), (y :: ys') ->
      f i x y;
      go (i + 1) xs' ys'
    | _ -> ()
  in go 0 xs ys

let rec update xs x' ~pred =
  match xs with
  | [] -> []
  | x :: xs ->
    if pred x then x' :: update xs x' ~pred
    else x :: update xs x' ~pred

let rec fold_right2 xs ys ~init ~f =
  match xs, ys with
  | (x :: xs'), (y :: ys') -> fold_right2 xs' ys' ~init ~f |> f x y
  | _ -> init

let rec fold3 xs ys zs ~init ~f =
  match xs, ys, zs with
  | (x :: xs'), (y :: ys'), (z :: zs') ->
    fold3 xs' ys' zs' ~init:(f init x y z) ~f
  | _ -> init

let find_index xs ~f =
  let rec go i = function
    | [] -> -1
    | x :: xs ->
      if f x then i
      else go (i + 1) xs
  in
    go 0 xs