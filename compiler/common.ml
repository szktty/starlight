open Core

module List = struct
  include Core.List
  include Xlist 
end

module Sexp = struct
  include Core.Sexp
  include Xsexp
end

let extract = Seplist.values
let printf = Printf.printf
let sprintf = Printf.sprintf

let satom name = Sexp.Atom name
let slist list = Sexp.List list
let spair name value = Sexp.List [satom name; value]
let sconcat list =
  List.fold_left list
    ~init:[]
    ~f:(fun accu e -> match e with
        | Some e -> e :: accu
        | None -> accu)
  |> List.rev
  |> slist
