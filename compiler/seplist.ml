open Core

type ('a, 'b) t =
  | Cons of 'a * ('b, 'a) t
  | Nil

let empty = Nil

let one e = Cons (e, Nil)

let cons e ~sep es = Cons (e, Cons (sep, es))

let hd = function
  | Nil -> None
  | Cons (e, _) -> Some e

let hd_exn es = Option.value_exn (hd es)

let tl es =
  match es with
  | Nil -> None
  | Cons (_, Nil) -> Some (None, es)
  | Cons (_, Cons (sep, es)) -> Some (Some sep, es)

let tl_exn es = Option.value_exn (tl es)

let rev es =
  let rec f es accu =
    match es with
    | Nil -> Nil
    | Cons (e, Nil) -> Cons (e, accu)
    | Cons (_e, Cons (_sep, Nil)) -> failwith "error"
    | Cons (e, Cons (sep, es)) ->
      f es @@ Cons (sep, Cons (e, accu))
  in
  f es Nil

let fold_left es ~init ~f =
  let rec fold_left' es accu =
    match es with
    | Nil -> accu
    | Cons (e, Nil) -> f accu None e
    | Cons (_e, Cons (_sep, Nil)) -> failwith "error"
    | Cons (e, Cons (sep, es)) ->
      fold_left' es @@ f accu (Some sep) e
  in
  fold_left' es init

let length es =
  fold_left es ~init:0 ~f:(fun accu _sep _e -> accu + 1)

let iter es ~f =
  fold_left es ~init:() ~f:(fun _accu sep e -> f sep e)

let iteri es ~f =
  ignore @@ fold_left es
    ~init:0
    ~f:(fun i sep e ->
        f i sep e;
        i + 1)

let opt_iter es_opt ~f =
  Option.iter es_opt ~f:(fun es -> iter es ~f)

let values es =
  fold_left es ~init:[] ~f:(fun accu _sep e -> e :: accu)
  |> List.rev
