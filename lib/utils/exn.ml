open Base

let unreachable () =
  let exception Unreachable in
  raise Unreachable