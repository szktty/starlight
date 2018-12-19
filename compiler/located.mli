type 'a t = {
  desc : 'a;
  loc : Location.t;
}

val create : Location.t -> 'a -> 'a t

val with_range : Location.t -> Location.t -> 'a -> 'a t

val start_line : 'a t -> int
