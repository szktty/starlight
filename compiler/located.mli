type 'a t = {
  desc : 'a;
  loc : Location.t;
}

val create : Location.t -> 'a -> 'a t

val desc : 'a t -> 'a

val loc : 'a t -> Location.t

val with_range : Location.t -> Location.t -> 'a -> 'a t

val start_line : 'a t -> int
