open Core

type t = {
  contents : string;
  length : int;
  lines : string array;
  line_offsets : int list;
}

let create contents =
  let length = String.length contents in
  let lines = String.split_lines contents |> Array.of_list in
  let line_offsets = String.foldi contents
      ~init:[0]
      ~f:(fun i lines c ->
          match c with
          | '\r' -> i :: lines
          | '\n' ->
            if i > 0 then begin
              match String.get contents (i-1) with
              | '\r' -> lines
              | _ -> i :: lines
            end else
              i :: lines
          | _ -> lines)
  in
  { contents; length; lines; line_offsets }

let is_space = function
  | ' ' | '\t' -> true
  | _ -> false

let is_newline = function
  | '\r' | '\n' -> true
  | _ -> false

let is_ignored c =
  is_space c || is_newline c

let is_comment c = c = '%'

let is_token c =
  not @@ is_space c || is_newline c || is_comment c

let rec next_char ?(space=false) ?(newline=false) file i =
  let i = i + 1 in
  if i+1 < file.length then
    match String.get file.contents i with
    | c when (is_space c && not space) ||
             (is_newline c && not newline) ->
      next_char ~space ~newline file (i+1)
    | ' ' -> Some `Space
    | '\t' -> Some `Tab
    | '%' -> Some `Comment
    | c when is_newline c -> Some `Newline
    | c -> Some `Other
  else
    None

type comment = [
  | `Continue
  | `Newlines
  | `Comment of string option * string list (* eol_comment * bol_comment *)
]

let strip_comment s =
  String.lstrip s ~drop:(fun c -> c = '%')
  |> String.lstrip ~drop:(fun c -> c = ' ')

let comment file i =
  let rec next i state =
    let i' = i + 1 in
    if i' < file.length then begin
      match (state, String.get file.contents i') with
      | `Continue, c when is_space c ->
        next i' state
      | `Continue, c when is_comment c ->
        next i' @@ `Eol_comment (Buffer.create 16)
      | `Continue, c when is_newline c ->
        next i' @@ `Bol (None, [])
      | `Eol_comment buf, c when is_newline c ->
        next i' @@ `Bol (Some (Buffer.contents buf), [])
      | `Eol_comment buf, c ->
        Buffer.add_char buf c;
        next i' (`Eol_comment buf)
      | `Bol (eol, bols), c when is_comment c ->
        next i' @@ `Bol_comment (eol, bols, Buffer.create 16)
      | `Bol (eol, bols), c when is_ignored c ->
        next i' @@ `Bol (eol, bols)
      | `Bol_comment (eol, bols, buf), c when is_newline c ->
        next i' @@ `Bol (eol, Buffer.contents buf :: bols)
      | `Bol_comment (eol, bols, buf), c ->
        Buffer.add_char buf c;
        next i' @@ `Bol_comment (eol, bols, buf)
      | _ -> state
    end else
      state
  in

  let strip_eol comment =
    Option.value_map comment
      ~default:comment
      ~f:(fun s -> Some (strip_comment s))
  in

  match next i `Continue with
  | `Continue ->
    `Continue
  | `Eol_comment buf ->
    `Comment (Some (Buffer.contents buf |> strip_comment), [])
  | `Bol (None, []) ->
    `Newlines
  | `Bol (eol, bols) ->
    `Comment (strip_eol eol,
              List.rev_map bols ~f:strip_comment)
  | `Bol_comment (eol, bols, buf) ->
    `Comment (strip_eol eol,
              List.rev_map (Buffer.contents buf :: bols) ~f:strip_comment)

let module_comment file =
  match comment file 0 with
  | `Comment (Some eol, bols) -> Some (eol :: bols)
  | _ -> None
