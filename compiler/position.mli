type t = {
  line : int;
  col : int;
  offset : int;
}

val zero : t

val of_lexing_pos : Lexing.position -> t

val add : t -> length:int -> t
val newline : t -> t

val to_string : t -> string
