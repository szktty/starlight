open Core
open Common

type t = {
  name : string;
  exports : (string * int) list;
  records : record list;
}

and record = {
  rec_name : string;
  rec_fields : string list;
}

let g_mods : t String.Map.t ref = ref String.Map.empty

let empty = { name = ""; exports = []; records = [] }

let add m =
  g_mods := String.Map.add_exn !g_mods ~key:m.name ~data:m

let get name =
  String.Map.find !g_mods name

let get_rec m name =
  List.find m.records ~f:(fun r -> r.rec_name = name)

let get_field r name : int option =
  List.findi r.rec_fields ~f:(fun _ name' -> name = name')
  |> Option.map ~f:(fun (i, _) -> i)
