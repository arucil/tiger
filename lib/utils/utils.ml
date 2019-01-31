
let rec iter2 f xs ys =
  match xs, ys with
  | (x :: xs'), (y :: ys') ->
    f x y;
    iter2 f xs' ys'
  | _ -> ()

let iter2i f xs ys =
  let rec go i xs ys =
    match xs, ys with
    | (x :: xs'), (y :: ys') ->
      f i x y;
      go (i + 1) xs' ys'
    | _ -> ()
  in go 0 xs ys

let rec update f x' = function
  | [] -> []
  | x :: xs ->
    if f x then x' :: update f x' xs
    else x :: update f x' xs

let rec fold_right2 f z xs ys =
  match xs, ys with
  | (x :: xs'), (y :: ys') -> fold_right2 f z xs' ys' |> f x y
  | _ -> z

let unreachable () =
  let exception Unreachable in
  raise Unreachable