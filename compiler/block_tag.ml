type t =
  | String
  | Binary
  | Bitstr
  | List
  | Tuple

let to_string = function
  | String -> "string"
  | Binary -> "binary"
  | Bitstr -> "bitstr"
  | List -> "list"
  | Tuple -> "tuple"

let to_repr = function
  | String -> "String"
  | Binary -> "Binary"
  | Bitstr -> "Bitstr"
  | List -> "List"
  | Tuple -> "Tuple"
