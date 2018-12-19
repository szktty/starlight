open Common
open Core

type ('value, 'size) t = {
  value : 'value;
  size : 'size;
  spec : spec;
}

and spec = {
  ty : [ `Int | `Float | `Binary | `Bitstr | `Utf8 | `Utf16 | `Utf32 ];
  sign : [ `Signed | `Unsigned ];
  endian : [ `Big | `Little | `Native ];
  unit : int option;
}

let create ~value ~size ~ty ~sign ~endian ~unit =
  { value; size; spec = { ty; sign; endian; unit } }

module Parser = struct

  let split_types ty =
    String.split ~on:'-' ty

  let ty ty =
    List.find_map (split_types ty)
      ~f:(function
          | "integer" -> Some `Int
          | "float" -> Some `Float
          | "binary" | "bytes" -> Some `Binary
          | "bitstring" | "bits" -> Some `Bitstr
          | "utf8" -> Some `Utf8
          | "utf16" -> Some `Utf16
          | "utf32" -> Some `Utf32
          | _ -> None)
    |> Option.value ~default:`Int

  let size ty =
    List.find_map (split_types ty)
      ~f:(function
          | "integer" -> Some 8
          | "float" -> Some 64
          | "binary" | "bytes" -> Some 2
          | "bitstring" | "bits" -> Some 1
          | _ -> None)
    |> Option.value ~default:8

  let sign ty =
    List.find_map (split_types ty)
      ~f:(function
          | "signed" -> Some `Signed
          | "unsigned" -> Some `Unsigned
          | _ -> None)
    |> Option.value ~default:`Signed

  let endian ty =
    List.find_map (split_types ty)
      ~f:(function
          | "big" -> Some `Big
          | "little" -> Some `Little
          | "native" -> Some `Native
          | _ -> None)
    |> Option.value ~default:`Big

  let unit ty =
    List.find_map (split_types ty)
      ~f:(fun spec ->
          match String.chop_prefix spec ~prefix:"unit:" with
          | None -> None
          | Some size -> Some (Int.of_string size))

end

module Repr = struct

  let type_name bits =
    match bits.ty with
    | `Int -> "int"
    | `Float -> "float"
    | `Binary -> "bin"
    | `Bitstr -> "bits"
    | `Utf8 -> "utf8"
    | `Utf16 -> "utf16"
    | `Utf32 -> "utf32"

  let sign_name bits =
    match bits.sign with
    | `Signed -> "s"
    | `Unsigned -> "u"

  let endian_name bits =
    match bits.endian with
    | `Big -> "be"
    | `Little -> "le"
    | `Native -> "ne"

  let spec_to_string spec =
    sprintf "%s-%s-%s:%s" 
      (type_name spec) (sign_name spec) (endian_name spec)
      (Option.value_map spec.unit ~default:"?"
         ~f:(fun unit -> Int.to_string unit))

end
