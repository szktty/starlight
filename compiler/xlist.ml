open Core

let unpack = function
  | [] -> None
  | e :: es -> Some (e, es)

let unpack_exn = function
  | [] -> failwith "empty"
  | e :: es -> e, es

let unpack_foldr list ~init ~f =
  let e, es = unpack_exn list in
  List.fold_right es ~init:(init e) ~f

let empty_map list ~default ~f =
  match list with
  | [] -> default
  | es -> f es

