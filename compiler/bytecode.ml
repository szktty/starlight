open Core

let version = 1

let extension = ".strlc"

module Code = struct

  type pc_loc = {
    pc_start : int;
    pc_end : int;
    pc_loc : Location.t;
  }

  type t = {
    name : string option;
    arity : int;
    consts : const list;
    ops : Opcode_t.t list;
    locals : int;
    frame : int;
    locs : pc_loc list;
    comments : string list Int.Map.t;
  }

  and const =
    | Const_atom of string
    | Const_string of string
    | Const_int of string
    | Const_float of string
    | Const_fun of t
    | Const_block of Block_tag.t * const list
    | Const_bits of (int, int) Bitstr.t

  let assoc ty items =
    (*`Assoc (List.append [("type", `String ty)] items)*)
    `List (`String ty :: items)

  let tagged ty items =
    match items with
    | [item] -> `Assoc [(ty, item)]
    | _ -> `Assoc [(ty, `List items)]

  let equal_consts a b =
    match a, b with
    | Const_atom a, Const_atom b -> String.equal a b
    | Const_string a, Const_string b -> String.equal a b
    | Const_int a, Const_int b -> String.equal a b
    | _ -> false

  let bits_spec_json (spec : Bitstr.spec) =
    let open Bitstr.Repr in
    let ty = match spec.ty with
      | `Int -> "Int"
      | `Float -> "Float"
      | `Binary -> "Binary"
      | `Bitstr -> "Bitstr"
      | `Utf8 -> "Utf8"
      | `Utf16 -> "Utf16"
      | `Utf32 -> "Utf32"
    in
    let sign = match spec.sign with
      | `Signed -> "Signed"
      | `Unsigned -> "Unsigned"
    in
    let endian = match spec.endian with
      | `Big -> "Big"
      | `Little -> "Little"
      | `Native -> "Native"
    in
    [`String ty; `String sign; `String endian;
     Option.value_map spec.unit
       ~default:`Null
       ~f:(fun unit -> `Int unit)]

  let rec const_to_json = function
    | Const_atom s -> tagged "Atom" [`String s]
    | Const_string s -> tagged "String" [`String s]
    | Const_int s -> tagged "Int" [`String s]
    | Const_float s -> tagged "Float" [`String s]
    | Const_fun code -> tagged "Function" [to_json code]
    | Const_block (tag, elts) ->
      tagged "Block" [`String (Block_tag.to_repr tag);
                      `List (List.map elts ~f:const_to_json)]
    | Const_bits bits ->
      tagged "Bitstr" (List.append
                         [`Int bits.value; `Int bits.size]
                         (bits_spec_json bits.spec))

  and to_json code =
    let rec op_to = function
      | Opcode_t.Nop -> `String "Nop"
      | Ref op -> op_to !op
      | Load_bool true -> `String "LoadTrue"
      | Load_bool false -> `String "LoadFalse"
      | Load_atom `Undef -> `String "LoadUndef"
      | Load_atom `Nil -> `String "LoadNil"
      | Load_const i -> tagged "LoadConst" [`Int i]
      | Load_int value -> tagged "LoadInt" [`Int value]
      | Load_local i -> tagged "LoadLocal" [`Int i]
      | Load_bitstr (size, value) ->
        tagged "LoadBitstr" [`Int size; `Int value]
      | Load_native_bitstr (size, value) ->
        tagged "LoadNativeBitstr" [`Int size; `Int value]
      | Load_ok n -> tagged "LoadOk" [`Int n]
      | Load_error n -> tagged "LoadError" [`Int n]
      | Load_pid -> `String "LoadPid"
      | Store_pop_local i -> tagged "StorePopLocal" [`Int i]
      | Get_global -> `String "GetGlobal"
      | Set_global -> `String "SetGlobal"
      | Branch (true, size) -> tagged "BranchTrue" [`Int !size]
      | Branch (false, size) -> tagged "BranchFalse" [`Int !size]
      | Jump size -> tagged "Jump" [`Int !size]
      | Loophead -> `String "Loophead"
      | No_match -> `String "NoMatch"
      | Create_block (tag, size) ->
        tagged "CreateBlock" [`String (Block_tag.to_repr tag); `Int size]
      | Get_block_field (Some i) -> tagged "GetBlockField" [`Int i]
      | Get_block_field None -> `String "XGetBlockField"
      | Set_block_field (Some i) -> tagged "SetBlockField" [`Int i]
      | Set_block_field None -> `String "XSetBlockField"
      | Get_block_size -> `String "GetBlockSize"
      | Test_block tag ->
        tagged "TestBlock" [`String (Block_tag.to_repr tag)]
      | Clos i ->
        tagged "Closure" [`Int i]
      | Create_bitstr spec ->
        tagged "CreateBitstr" (bits_spec_json spec)
      | Create_rec (i, n) ->
        tagged "CreateRecord" [`Int i; `Int n]
      | Update_rec n ->
        tagged "UpdateRecord" [`Int n]
      | Get_rec_field i ->
        tagged "GetRecordField" [`Int i]
      | Apply nargs -> tagged "Apply" [`Int nargs]
      | Spawn -> `String "Spawn"
      | Return -> `String "Return"
      | Pop -> `String "Pop"
      | Not -> `String "Not"
      | Eq -> `String "Eq"
      | Ne -> `String "Ne"
      | Lt -> `String "Lt"
      | Add -> `String "Add"
      | Add1 -> `String "Add1"
      | Sub -> `String "Sub"
      | Mul -> `String "Mul"
      | Rem -> `String "Rem"
      | List_len -> `String "ListLen"
      | List_cons -> `String "ListCons"
      | List_concat -> `String "ListConcat"
      | List_sub -> `String "ListSub"
      | Test_nonnil -> `String "TestNonNil"
      | _ -> failwith "notimpl"
    in
    let ops = List.map code.ops ~f:op_to in
    `Assoc [("name", Option.value_map code.name
               ~default:`Null
               ~f:(fun name -> `String name));
            ("arity", `Int code.arity);
            ("locals", `Int code.locals);
            ("frame", `Int code.frame);
            ("consts", `List (List.map code.consts ~f:const_to_json));
            ("locs", `List
               (List.map code.locs ~f:(fun loc ->
                    `Assoc [("start", `Int loc.pc_start);
                            ("end", `Int loc.pc_end);
                            ("loc",
                             `List [
                               `Assoc [("line", `Int loc.pc_loc.start.line);
                                       ("col", `Int loc.pc_loc.start.col);
                                       ("offset", `Int loc.pc_loc.start.offset)];
                               `Assoc [("line", `Int loc.pc_loc.end_.line);
                                       ("col", `Int loc.pc_loc.end_.col);
                                       ("offset", `Int loc.pc_loc.end_.offset)]
                             ])])));
            ("opcodes", `List ops)]

  let rec to_debug code =
    let open Buffer in
    let open Printf in
    let buf = create 100 in
    let rec f_code code =
      List.iter code.consts ~f:(function
          | Const_fun fn -> f_code fn
          | _ -> ());
      add_string buf (sprintf "function: %s/%d\n"
                        (Option.value code.name ~default:"fun")
                        code.arity);
      add_string buf (sprintf "locals: %d\n" code.locals);
      add_string buf (sprintf "frame size: %d\n" code.frame);
      add_string buf
        (sprintf "constants: (%d)\n%s\n"
           (List.length code.consts)
           (String.concat  
              (List.mapi code.consts
                 ~f:(fun i const ->
                     sprintf "    % 3d: %s" i (f_const code const)))
              ~sep:"    \n"));
      add_string buf
        (sprintf "opcodes: (%d)\n%s\n"
           (List.length code.ops)
           (Opcode.Debug.to_string
              { const = (fun i ->
                    (List.nth_exn code.consts i |> f_const code));
                comment = (fun pc ->
                    Map.find_multi code.comments pc
                    |> List.rev);
              } code.ops))
    in
    f_code code;
    contents buf

  and f_const code = function
    | Const_atom name -> sprintf "#%s" name
    | Const_int s  -> s
    | Const_string s  -> sprintf "\"%s\"" s
    | Const_fun fn ->
      sprintf "%s/%d" 
        (Option.value fn.name ~default:"fun")
        code.arity
    | Const_block (Block_tag.List, elts) ->
      sprintf "[%s]" ((List.map elts ~f:(f_const code)
                       |> String.concat ~sep:", "))
    | Const_block (Block_tag.Tuple, elts) ->
      sprintf "{%s}" ((List.map elts ~f:(f_const code)
                       |> String.concat ~sep:", "))
    | Const_block (tag, elts) ->
      sprintf "[%s: %s]"
        (Block_tag.to_string tag)
        ((List.map elts ~f:(f_const code)
          |> String.concat ~sep:", "))
    | Const_bits bits ->
      let open Bitstr.Repr in
      sprintf "<<%d:%d/%s-%s-%s>>" bits.value bits.size
        (type_name bits.spec) (sign_name bits.spec) (endian_name bits.spec)
    | _ -> "?"

end

module Module = struct

  type t = {
    mutable name : string;
    mutable auths : string list;
    mutable exports : (string * int) list;
  }

end

type t = {
  version : int;
  m : Module.t;
  main : Code.t;
}

let filename file =
  let dir, base = Filename.split file in
  let body, _ = Filename.split_extension base in
  body, Filename.concat dir (body ^ extension)

let bc_filename file =
  filename file |> Tuple2.get2

let from_code file m code =
  bc_filename file, { version = 1;
                      m;
                      main = code }

let to_json (bc : t) =
  `Assoc [("version", `Int bc.version);
          ("module", `String bc.m.name);
          ("authors", `List (List.map bc.m.auths
                               ~f:(fun name -> `String name)));
          ("exports", `List
             (List.map bc.m.exports
                ~f:(fun (name, arity) ->
                    `List [`String name; `Int arity])));
          ("main", Code.to_json bc.main)]

let write file bc =
  let open Out_channel in
  with_file file
    ~f:(fun chan ->
        Yojson.Basic.pretty_to_channel chan bc)
