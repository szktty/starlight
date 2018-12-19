type t =
  | String
  | Binary
  | Bitstr
  | List
  | Tuple
  | Process
  | Module

let to_string = function
  | String -> "string"
  | Binary -> "binary"
  | Bitstr -> "bitstr"
  | List -> "list"
  | Tuple -> "tuple"
  | Process -> "process"
  | Module -> "module"

let to_repr = function
  | String -> "String"
  | Binary -> "Binary"
  | Bitstr -> "Bitstr"
  | List -> "List"
  | Tuple -> "Tuple"
  | Process -> "Process"
  | Module -> "Module"
