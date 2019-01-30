open Core

type 'a t = {
  desc : 'a;
  loc : Location.t;
}

let create loc desc =
  { desc; loc }

let desc locd = locd.desc

let loc locd = locd.loc

let with_range start_loc end_loc desc =
  create (Location.union start_loc end_loc) desc

let start_line locd =
  Position.(locd.loc.start.line)
