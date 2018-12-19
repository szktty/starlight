open Core

type t = {
  line : int;
  col : int;
  offset : int;
}

let zero = { line = 0; col = 0; offset = 0 }

let of_lexing_pos (pos : Lexing.position) =
  { line = pos.pos_lnum;
    col = pos.pos_bol;
    offset = pos.pos_cnum;
  }

let add pos ~length =
  { pos with col = pos.col + length;
             offset = pos.offset + length }

let newline pos =
  { line = pos.line + 1;
    col = 0;
    offset = pos.offset + 1;
  }

let to_string pos =
  Printf.sprintf "%d:%d:%d" (pos.line + 1) pos.col pos.offset
