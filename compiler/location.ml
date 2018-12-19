open Core

type t = {
  start : Position.t;
  end_ : Position.t;
  len : int;
}

let create (start : Position.t) (end_ : Position.t) =
  let len = end_.offset - start.offset in
  if len < 0 then
    raise (Invalid_argument
             (sprintf "Location.create: start > end (%d, %d)"
                start.offset end_.offset))
  else
    { start; end_; len }

let zero =
  create Position.zero Position.zero

let offset loc =
  loc.start.offset

let union start end_ =
  create start.start end_.end_

let contains_pos loc (pos : Position.t) =
  loc.start.offset <= pos.offset && pos.offset < loc.end_.offset

let contains_offset loc offset =
  loc.start.offset <= offset && offset < loc.end_.offset

let to_string loc =
  Printf.sprintf "%s-%s-%d"
    (Position.to_string loc.start)
    (Position.to_string loc.end_)
    loc.len
