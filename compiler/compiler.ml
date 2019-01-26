open Core 

module Context = struct

  type t = {
    id : int;
    name : string option;
    mutable arity : int;
    mutable st_base : int;
    mutable st_top : int;
    mutable vars : string list;
    mutable consts : const list;
    mutable ops : Opcode_t.t list;
    mutable frame : int;
    mutable exits : exit Int.Map.t;
    mutable exns : int list;
    mutable locs : Bytecode.Code.pc_loc list;
    mutable comments : string list Int.Map.t;
  }

  and const =
    | Const_atom of string
    | Const_string of string
    | Const_int of string
    | Const_float of string
    | Const_fun of t
    | Const_block of Block_tag.t * const list
    | Const_bits of (int, int) Bitstr.t
    | Const_bits_spec of Bitstr.spec

  and exit = {
    mutable ex_src : int;
    mutable ex_dest : int;
    ex_size : int ref;
  }

  let next_id = ref 0

  let create ~name ~arity ~st_base =
    next_id := !next_id + 1;
    { id = !next_id;
      name;
      arity;
      st_base;
      st_top = 0;
      vars = [];
      consts = [];
      ops = [];
      frame = 0;
      exits = Int.Map.empty;
      exns = [];
      locs = [];
      comments = Int.Map.empty }

  let push ctx =
    ctx.st_top <- ctx.st_top + 1;
    if ctx.frame < ctx.st_top then
      ctx.frame <- ctx.st_top

  let pop ctx =
    ctx.st_top <- ctx.st_top - 1

  let popn ctx n =
    ctx.st_top <- ctx.st_top - n

  let def_var ctx name =
    match List.find ctx.vars ~f:(String.equal name) with
    | None -> ctx.vars <- name :: ctx.vars
    | _ -> ()

  let vars ctx =
    List.rev ctx.vars

  let fun_name ctx =
    Option.value_map ctx.name
      ~default:(sprintf "?/%d" ctx.arity)
      ~f:(fun name -> sprintf "%s/%d" name ctx.arity)

  let get_var ctx name =
    match List.findi (vars ctx)
            ~f:(fun i name0 -> String.equal name0 name) with
    | Some (i, _) -> i
    | None -> failwith (sprintf "var '%s' is not found" name)

  let equal_consts a b =
    match a, b with
    | Const_atom a, Const_atom b -> String.equal a b
    | Const_string a, Const_string b -> String.equal a b
    | Const_int a, Const_int b -> String.equal a b
    | Const_fun a, Const_fun b -> a.id = b.id
    | _ -> a = b

  let add_const ctx const =
    match List.findi ctx.consts ~f:(fun _ exist ->
        equal_consts exist const) with
    | Some (i, _) -> i
    | _ ->
      ctx.consts <- List.append ctx.consts [const];
      (List.length ctx.consts) - 1

  let last_pc ctx =
    List.length ctx.ops - 1

  let next_pc ctx =
    List.length ctx.ops

  let add_exit ctx n =
    ctx.exits <- Int.Map.add_exn ctx.exits
        ~key:n
        ~data:{ ex_src = 0; ex_dest = 0; ex_size = ref 0 }

  let exit_size ctx n =
    let exit = Int.Map.find_exn ctx.exits n in
    exit.ex_size

  let set_exit_src ctx n pc =
    let exit = Int.Map.find_exn ctx.exits n in
    exit.ex_src <- pc

  let set_exit_dest ctx n pc =
    let exit = Int.Map.find_exn ctx.exits n in
    exit.ex_dest <- pc;
    exit.ex_size := exit.ex_dest - exit.ex_src 

  let add_atom ctx name =
    match List.findi ctx.consts ~f:(fun _ exist ->
        match exist with
        | Const_atom exist -> String.equal exist name
        | _ -> false) with
    | Some (i, _) -> i
    | _ -> add_const ctx (Const_atom name)

  let add_string ctx name =
    match List.findi ctx.consts ~f:(fun _ exist ->
        match exist with
        | Const_string exist -> String.equal exist name
        | _ -> false) with
    | Some (i, _) -> i
    | _ -> add_const ctx (Const_string name)

  let add_op ctx op =
    ctx.ops <- List.append ctx.ops [op]

  let add_op_push ctx op =
    add_op ctx op;
    push ctx

  let add_op_pop ctx op =
    add_op ctx op;
    pop ctx

  let add_op_pop2 ctx op =
    add_op ctx op;
    popn ctx 2

  let add_load_local ctx name =
    add_op_push ctx (Load_local (get_var ctx name))

  let add_load_const ctx i =
    add_op_push ctx (Load_const i)

  let add_load_undef ctx =
    add_op_push ctx (Load_atom `Undef)

  let add_load_int ctx value =
    add_op_push ctx (Load_int value)

  let add_store_local ctx name =
    add_op_pop ctx (Store_pop_local (get_var ctx name))

  let add_make_block ctx tag size =
    add_op_push ctx (Create_block (tag, size))

  let add_branch ctx flag dest =
    add_op_pop ctx (Branch (flag, dest))

  let add_loophead ctx =
    add_op ctx Loophead

  let add_jump ctx dest =
    add_op ctx (Jump dest)

  let add_comment ctx pc msg =
    ctx.comments <- Map.add_multi ctx.comments ~key:pc ~data:msg

  let rec to_code ctx =
    { Bytecode.Code.name = ctx.name;
      arity = ctx.arity;
      consts = List.map ctx.consts ~f:const_to_code;
      ops = ctx.ops;
      locals = List.length ctx.vars;
      frame = ctx.frame;
      locs = List.rev ctx.locs;
      comments = ctx.comments;
    }

  and const_to_code = function
    | Const_atom s -> Bytecode.Code.Const_atom s
    | Const_int s -> Const_int s
    | Const_string s -> Const_string s
    | Const_fun ctx -> Const_fun (to_code ctx)
    | Const_block (tag, elts) ->
      Const_block (tag, List.map elts ~f:const_to_code)
    | Const_bits bits -> Const_bits bits
    | _ -> Const_atom "?"

end

let compile form =
  let open Context in
  let open Printf in
  let mod_base : Lambda_t.module_ option ref = ref None in
  let meta = Id.gen () in

  let rec f ctx = function
    | Lambda_t.Nop ->
      add_op ctx Nop

    | Event ev ->
      let start = next_pc ctx in
      f ctx ev.ev_exp;
      let end_ = last_pc ctx in
      ctx.locs <- { pc_start = start;
                    pc_end = end_;
                    pc_loc = ev.ev_loc } :: ctx.locs

    | Module m ->
      mod_base := Some m;
      add_comment ctx (next_pc ctx) "begin module";
      f ctx m.mod_code;
      add_comment ctx (last_pc ctx) "end module"

    | Catch (exp, exits) ->
      let id = Id.next meta "*catch*" in
      add_comment ctx (next_pc ctx) (sprintf "begin %s" id);
      List.iter exits ~f:(fun (n, _) -> add_exit ctx n);

      add_comment ctx (next_pc ctx) (sprintf "catch %s" id);
      f ctx exp;
      let to_end = ref (next_pc ctx) in
      add_comment ctx (next_pc ctx) (sprintf "to end %s" id);
      add_op ctx (Jump to_end);

      let exit_to_end = List.map exits
          ~f:(fun (n, body) ->
              set_exit_dest ctx n (next_pc ctx);
              add_comment ctx (next_pc ctx) (sprintf "from exit %d" n);
              f ctx body;
              let to_end = ref (next_pc ctx) in
              add_comment ctx (next_pc ctx) (sprintf "to end %s" id);
              add_op ctx (Jump to_end);
              to_end) in

      let end_pc = next_pc ctx in
      to_end := end_pc - !to_end;
      List.iter exit_to_end ~f:(fun exit -> exit := end_pc - !exit)

    | Exit n ->
      set_exit_src ctx n (next_pc ctx);
      add_comment ctx (next_pc ctx) (sprintf "catch %d" n);
      let size = exit_size ctx n in
      add_comment ctx (next_pc ctx) (sprintf "exit %d" n);
      add_op ctx (Jump size)

    | If (cond, then_, else_) ->
      let id = Id.next meta "*if*" in
      add_comment ctx (next_pc ctx) ("begin " ^ id);
      f ctx cond;

      add_comment ctx (next_pc ctx) ("to else " ^ id);
      let to_else = ref (next_pc ctx) in
      add_branch ctx false to_else;

      add_comment ctx (next_pc ctx) ("then " ^ id);
      f ctx then_;

      add_comment ctx (next_pc ctx) ("to end " ^ id);
      let to_end = ref (next_pc ctx) in
      add_jump ctx to_end;

      add_comment ctx (next_pc ctx) ("else " ^ id);
      to_else := next_pc ctx - !to_else;
      f ctx else_;
      add_comment ctx (last_pc ctx) ("end " ^ id);
      to_end := next_pc ctx - !to_end

    | Loop (cond, body) ->
      let id = Id.next meta "*loop*" in

      (* loophead *)
      add_comment ctx (next_pc ctx) (sprintf "begin %s" id);
      let to_begin = ref (next_pc ctx) in
      add_loophead ctx;

      (* loop *)
      add_comment ctx (next_pc ctx) (sprintf "%s condition" id);
      let to_end = ref (next_pc ctx) in
      f ctx cond;
      add_branch ctx false to_end;
      add_comment ctx (next_pc ctx) (sprintf "%s body" id);
      f ctx body;
      to_begin := !to_begin - next_pc ctx;
      add_op ctx (Jump to_begin);

      to_end := last_pc ctx - !to_end;
      add_comment ctx (last_pc ctx) (sprintf "end %s" id);

    | Let (vars, body) ->
      let id = Id.next meta "*block*" in
      add_comment ctx (last_pc ctx) ("begin " ^ id);
      List.iter vars ~f:(fun (var, _) -> def_var ctx var);
      List.iter vars
        ~f:(fun (var, exp) ->
            add_comment ctx (next_pc ctx) ("let value " ^ var);
            f ctx exp;
            add_comment ctx (next_pc ctx) ("let assign " ^ var);
            add_store_local ctx var);
      add_comment ctx (next_pc ctx) ("let body " ^ id);
      f ctx body;
      add_comment ctx (last_pc ctx) ("end " ^ id)

    | Seq (exp1, exp2) ->
      f ctx exp1;
      add_op_pop ctx Pop;
      f ctx exp2

    | Fun (name, params, body) ->
      let id = Option.map name ~f:(fun name -> Id.next meta name) in
      let ctx2 = create
          ~name:id
          ~arity:(List.length params)
          ~st_base:ctx.st_top in

      (* outer variables + arguments *)
      let locals = List.append (Context.vars ctx) params in
      List.iter locals ~f:(fun name -> def_var ctx2 name);

      (* store arguments on operand stack in local variables *)
      List.iter params ~f:(fun name -> add_store_local ctx2 name);

      f ctx2 body;
      add_op ctx2 Return;
      add_load_const ctx (add_const ctx (Const_fun ctx2))

    | Apply (fn, args) ->
      let id = Id.next meta "*apply*" in
      add_comment ctx (next_pc ctx) ("begin " ^ id);
      f ctx fn;
      List.iteri args
        ~f:(fun i arg ->
            add_comment ctx (next_pc ctx)
              (sprintf "begin arg %d for %s" (i + 1) id);
            f ctx arg;
            add_comment ctx (last_pc ctx)
              (sprintf "end arg %d for %s" (i + 1) id));
      add_comment ctx (next_pc ctx) ("end " ^ id);
      let nargs = List.length args in
      add_op ctx (Apply nargs);
      popn ctx nargs

    | Spawn (fn, args) ->
      let id = Id.next meta "*spawn*" in
      add_comment ctx (next_pc ctx) (sprintf "%s fun" id);
      f ctx fn;
      add_comment ctx (next_pc ctx) (sprintf "%s args" id);
      f ctx args;
      add_op_pop ctx Spawn

    | Get_global name ->
      add_comment ctx (next_pc ctx) "get global";
      f ctx name;
      add_op ctx Get_global

    | Get_prop (map, name) ->
      add_comment ctx (next_pc ctx) "get prop map";
      f ctx map;
      add_comment ctx (next_pc ctx) "get prop";
      f ctx name;
      add_op_pop ctx (Get_block_field None)

    | Get_field (ary, i) ->
      add_comment ctx (next_pc ctx) (sprintf "get field at %d" i);
      f ctx ary;
      add_op_pop ctx (Get_block_field (Some i))

    | Get_bitstr (value, spec, pos) ->
      add_comment ctx (next_pc ctx) "get bitstr value";
      f ctx value;
      add_comment ctx (next_pc ctx) "get bitstr pos";
      f ctx pos;
      add_op ctx (Get_bitstr spec)

    | Set_global (name, exp) ->
      add_comment ctx (next_pc ctx) "set global name";
      f ctx name;
      add_comment ctx (next_pc ctx) "set global value";
      f ctx exp;
      add_op_pop ctx Set_global

    | Set_local (name, exp) ->
      let i = get_var ctx name in
      add_comment ctx (next_pc ctx) (sprintf "set local %d" i);
      f ctx exp;
      add_store_local ctx name

    | Update_rec up ->
      add_comment ctx (next_pc ctx) "create record";
      let count = List.length up.up_assocs in
      begin match up.up_exp with
        | None ->
          let name_i = add_atom ctx up.up_name in
          List.iter up.up_assocs
            ~f:(fun (k, v) ->
                add_load_const ctx (add_atom ctx up.up_name);
                f ctx v);
          popn ctx (count * 2);
          add_op_push ctx (Create_rec (name_i, count))
        | Some exp ->
          f ctx exp;
          List.iter up.up_assocs
            ~f:(fun (k, v) ->
                add_load_const ctx (add_atom ctx up.up_name);
                f ctx v);
          popn ctx (count * 2 - 1);
          add_op_push ctx (Update_rec count)
      end

    | Block (tag, exps) ->
      begin match exps with
        | (Lambda_t.Bitstr bits) :: [] ->
          let bits = f_bits ctx bits in
          let op = match bits.spec.endian with
            | `Native ->
              Opcode_t.Load_native_bitstr (bits.size, bits.value)
            | _ -> Load_bitstr (bits.size, bits.value)
          in
          add_op_push ctx op
        | _ ->
          let blk = f_const_block ctx tag exps in
          add_load_const ctx (add_const ctx blk)
      end

    | Create_block (tag, exps) ->
      let id = Id.next meta "*block*" in
      List.iteri exps ~f:(fun i exp ->
          add_comment ctx (next_pc ctx) (sprintf "prepare %s[%d]" id i);
          f ctx exp);
      popn ctx (List.length exps);
      add_comment ctx (next_pc ctx) (sprintf "make %s" id);
      add_op_push ctx (Create_block (tag, List.length exps))

    | Temp_block _ ->
      failwith "error"

    | Create_bitstr bits ->
      add_comment ctx (next_pc ctx) "bitstr value";
      f ctx bits.value;
      add_comment ctx (next_pc ctx) "bitstr size";
      f ctx bits.size;
      add_op ctx (Create_bitstr bits.spec)

    | No_match ->
      add_op_pop ctx No_match

    | Not exp ->
      f ctx exp;
      add_op ctx Not

    | Ne (a, b) -> f_binexp ctx a b Opcode_t.Ne
    | Add (a, b) -> f_binexp ctx a b Opcode_t.Add
    | Sub (a, b) -> f_binexp ctx a b Opcode_t.Sub
    | Rem (a, b) -> f_binexp ctx a b Opcode_t.Rem
    | List_concat (a, b) -> f_binexp ctx a b Opcode_t.List_concat
    | List_sub (a, b) -> f_binexp ctx a b Opcode_t.List_sub

    | Block_size exp ->
      add_comment ctx (next_pc ctx) "get block size";
      f ctx exp;
      add_op ctx Get_block_size

    | Test_tuple exp ->
      add_comment ctx (next_pc ctx) "test tuple";
      f ctx exp;
      add_op ctx (Test_block Block_tag.Tuple)

    | Local name ->
      add_comment ctx (next_pc ctx) (sprintf "var $%s" name);
      begin match ctx.name with
        | Some self when String.equal self name ->
          add_op_push ctx Load_self_fun
        | _ -> add_load_local ctx name
      end

    | Bool b ->
      add_op_push ctx (Load_bool b)

    | Ok exps ->
      f_exps ctx exps;
      popn ctx (List.length exps);
      add_op_push ctx (Load_ok (List.length exps))

    | Error exps ->
      f_exps ctx exps;
      popn ctx (List.length exps);
      add_op_push ctx (Load_error (List.length exps))

    | Atom "undefined" ->
      add_op_push ctx (Load_atom `Undef)

    | Atom name ->
      add_load_const ctx (add_atom ctx name)

    | Int v ->
      add_op_push ctx (Load_int (Int.of_string v))

    | String s ->
      add_load_const ctx (add_string ctx s)

    | Test_nonnil exp ->
      f ctx exp;
      add_op_pop ctx Test_nonnil

    | op ->
      failwith (sprintf "notimpl %s\n" (Lambda.to_string op))

  and f_exps ctx exps =
    List.iter exps ~f:(f ctx)

  and f_binexp ctx a b (op : Opcode_t.t) =
    let name = match op with
      | Not -> "not"
      | Eq -> "="
      | Ne -> "!="
      | Add -> "+"
      | Sub -> "-"
      | Rem -> "%"
      | List_concat -> "++"
      | List_sub -> "--"
      | _ -> failwith "notimpl binop"
    in
    add_comment ctx (next_pc ctx) (sprintf "%s left" name);
    f ctx a;
    add_comment ctx (next_pc ctx) (sprintf "%s right" name);
    f ctx b;
    add_op_pop2 ctx op

  and f_const_block ctx tag exps =
    let f = function
      | Lambda_t.Bitstr bits -> Const_bits (f_bits ctx bits)
      | Atom name -> Const_atom name
      | Int s -> Const_int s
      | Block (tag, exps) -> f_const_block ctx tag exps
      | exp -> failwith (sprintf "invalid %s" (Lambda.to_string exp))
    in
    Const_block (tag, List.map exps f)   

  and f_bits ctx (bits : Lambda_t.bitstr) : (int, int) Bitstr.t =
    { bits with
      value = (match bits.value with
          | Int s -> Int.of_string s
          | _ -> failwith "constant bitstring value must be int");
      size = (match bits.size with
          | Int s -> Int.of_string s
          | _ -> failwith "constant bitstring size must be int")
    }

  in

  let ctx = Context.create
      ~name:(Some "__init")
      ~arity:0
      ~st_base:0 in
  f ctx form;
  add_op ctx (Load_atom `Undef);
  add_op ctx Return;

  let m = match !mod_base with
    | None -> failwith "module not found"
    | Some m -> m
  in
  { Bytecode.Module.name = Option.value_exn m.mod_name;
    auths = m.mod_authors;
    exports = m.mod_exports },
  Context.to_code ctx
