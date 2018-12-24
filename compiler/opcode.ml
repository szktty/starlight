open Core
open Opcode_t

module Debug = struct

  type t = {
    const : int -> string;
    comment : int -> string list;
  }

  let to_string fmt ops =
    let open Printf in
    let rec f pc = function
      | Nop -> "nop"
      | Ref op -> f pc !op
      | Load_local i -> sprintf "load local %d" i
      | Load_const i -> sprintf "load %s" (fmt.const i)
      | Load_int i -> sprintf "load %d" i
      | Load_true -> "load true"
      | Load_false -> "load false"
      | Load_undef -> "load #undefined"
      | Load_ok -> "load #ok"
      | Load_error -> "load #error"
      | Load_empty tag ->
        sprintf "load empty %s" (Block_tag.to_string tag)
      | Load_pid -> "load pid"
      | Load_context -> "load context"
      | Load_self_fun -> "load self function"
      | Load_bitstr (size, value) ->
        sprintf "load <<%d:%d>>" value size
      | Load_native_bitstr (size, value) ->
        sprintf "load <<%d:%d/native>>" value size
      | Store_pop_local i -> sprintf "store local %d; pop" i
      | Get_field i -> sprintf "get field %d" i
      | Get_prop -> "get property"
      | Get_global -> "get global"
      | Get_bitstr spec ->
        sprintf "get <<%s>>" (Bitstr.Repr.spec_to_string spec)
      | Set_field i -> sprintf "set field %d" i
      | Set_global -> sprintf "set global"
      | Return -> "return"
      | Return_true -> "return true"
      | Return_false -> "return false"
      | Return_ok -> "return #ok"
      | Return_error -> "return #error"
      | Return_undef -> "return #undefined"
      | Pop -> "pop"
      | No_match -> "no match"
      | Loophead -> "loophead"
      | Jump n -> sprintf "jump %d" (pc + !n)
      | Branch_true n -> sprintf "branch true %d" (pc + !n)
      | Branch_false n -> sprintf "branch false %d" (pc + !n)
      | Throw -> "throw"
      | Make_block (tag, size) ->
        sprintf "make %s (%d)" (Block_tag.to_string tag) size
      | Make_bitstr spec ->
        sprintf "make bitstr <<%s>>" (Bitstr.Repr.spec_to_string spec)
      | Make_fun (i, n) ->
        sprintf "make function with %d at %d" i n
      | Make_ok n -> sprintf "make #ok (%d)" n
      | Make_error n -> sprintf "make #error (%d)" n
      | Apply nargs -> sprintf "apply %d" nargs
      | Spawn -> "spawn"
      | Not -> "not"
      | Eq -> "=="
      | Ne -> "!="
      | Lt -> "<"
      | Add -> "+"
      | Add1 -> "+ 1"
      | Sub -> "-"
      | Mul -> "*"
      | Rem -> "rem"
      | Block_size -> "block size"
      | List_len -> "list length"
      | List_cons -> "construct list"
      | List_concat -> "++"
      | List_sub -> "--"
      | Test_tuple -> "test tuple"
      | Test_nonnil -> "test non nil"
    in
    let align = 32 in
    let descs = List.mapi ops
        ~f:(fun pc op ->
            let pc1 = pc + 1 in
            let base = Printf.sprintf "    % 3d: %s" pc1 (f pc1 op) in
            let len = String.length base in
            match fmt.comment pc with
            | [] -> base
            | msg :: msgs ->
              let align = if align < len then len else align in
              let indent = String.make align ' ' in
              let line =
                sprintf "%s%s // %s" base (String.make (align - len) ' ') msg in
              let lines = List.map msgs
                  ~f:(fun msg -> Printf.sprintf "%s // %s" indent msg) in
              String.concat (line :: lines) ~sep:"\n") in
    (String.concat ~sep:"\n" descs) ^ "\n"

end
