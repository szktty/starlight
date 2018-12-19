open Core

let version = "0.1"

let debug_mode = ref false
let verbose_mode = ref false

let debug f =
  if !debug_mode then
    printf ("# " ^^ f ^^ "\n")
  else
    Printf.ifprintf stderr f

let verbose f =
  if !verbose_mode || !debug_mode then
    printf ("# " ^^ f ^^ "\n")
  else
    Printf.ifprintf stderr f


