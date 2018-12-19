open Core

type t =
  | Comment of Ast_t.text
  | Newline of Ast_t.text

let g_annots : t list ref = ref []

let all () = !g_annots

let add annot =
  g_annots := annot :: !g_annots

let add_comment text =
  add @@ Comment text

let add_newline text =
  add @@ Newline text
