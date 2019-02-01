val unpack : 'a list -> ('a * 'a list) option

val unpack_exn : 'a list -> 'a * 'a list

val unpack_foldr : 'a list -> init:('a -> 'b) -> f:('a -> 'b -> 'b) -> 'b

val empty_map : 'a list -> default:'b -> f:('a list -> 'b) -> 'b
