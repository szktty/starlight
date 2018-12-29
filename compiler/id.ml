open Core

type t = string

type gen = int ref

let gen () =
  ref 0

let next gen name =
  gen := !gen + 1;
  Printf.sprintf "%s#%d" name !gen

let param gen =
  next gen "*p*"

let match_ gen =
  next gen "*m*"

let to_c id =
  String.substr_replace_all id ~pattern:"/" ~with_:"_"
