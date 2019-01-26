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
      | Load_bool b -> sprintf "load %s" (Bool.to_string b)
      | Load_atom `Undef -> "load #undefined"
      | Load_atom `Nil -> "load #nil"
      | Load_ok n -> sprintf "load #ok %d" n
      | Load_error n -> sprintf "load #error %d" n
      | Load_pid -> "load pid"
      | Load_context -> "load context"
      | Load_self_fun -> "load self function"
      | Load_bitstr (size, value) ->
        sprintf "load <<%d:%d>>" value size
      | Load_native_bitstr (size, value) ->
        sprintf "load <<%d:%d/native>>" value size
      | Store_pop_local i -> sprintf "store local %d; pop" i
      | Get_global -> "get global"
      | Set_global -> "set global"
      | Get_bitstr spec ->
        sprintf "get <<%s>>" (Bitstr.Repr.spec_to_string spec)
      | Return -> "return"
      | Pop -> "pop"
      | No_match -> "no match"
      | Loophead -> "loophead"
      | Jump n -> sprintf "jump %d" (pc + !n)
      | Branch (b, n) -> sprintf "branch %s %d" (Bool.to_string b) (pc + !n)
      | Throw -> "throw"
      | Create_block (tag, size) ->
        sprintf "create %s (%d)" (Block_tag.to_string tag) size
      | Get_block_field (Some i) -> sprintf "get block field %d" i
      | Get_block_field None -> "get block field"
      | Set_block_field (Some i) -> sprintf "set block field %d" i
      | Set_block_field None -> "set block field"
      | Get_block_size -> "get block size"
      | Test_block tag ->
        sprintf "test %s" (Block_tag.to_string tag)
      | Create_bitstr spec ->
        sprintf "create bitstr <<%s>>" (Bitstr.Repr.spec_to_string spec)
      | Create_clos (i, n) ->
        sprintf "create %s/%d" (fmt.const i) n
      | Create_rec (i, n) -> sprintf "create record %s %d" (fmt.const i) n
      | Update_rec n -> sprintf "update record %d" n
      | Get_rec_field i -> sprintf "get record field %s" (fmt.const i)
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
      | List_len -> "list length"
      | List_cons -> "construct list"
      | List_concat -> "++"
      | List_sub -> "--"
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
