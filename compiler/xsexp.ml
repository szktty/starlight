open Core

let tagged tag list =
  Sexp.List ([Sexp.Atom tag] @ list)

let to_string t ~f =
  Sexp.to_string_hum ~indent:2 (f t)
