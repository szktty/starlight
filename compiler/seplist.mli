type ('a, 'b) t

val empty : ('a, 'b) t
val one : 'a -> ('a, 'b) t
val cons : 'a -> sep:'b -> ('a, 'b) t -> ('a, 'b) t

val hd : ('a, 'b) t -> 'a option
val hd_exn : ('a, 'b) t -> 'a
val tl : ('a, 'b) t -> ('b option * ('a, 'b) t) option
val tl_exn : ('a, 'b) t -> 'b option * ('a, 'b) t
val values : ('a, 'b) t -> 'a list

val rev : ('a, 'b) t -> ('a, 'b) t
val length : ('a, 'b) t -> int

val iter : ('a, 'b) t -> f:('b option -> 'a -> unit) -> unit
val iteri : ('a, 'b) t -> f:(int -> 'b option -> 'a -> unit) -> unit
val fold_left : ('a, 'b) t -> init:'c -> f:('c -> 'b option -> 'a -> 'c) -> 'c
val opt_iter : ('a, 'b) t option -> f:('b option -> 'a -> unit) -> unit
