include Lambda0
include Lambda_to_sexp

open Core
open Common
open Located
open Lambda_t

let nomatch after = 
  ev_after (Ast.loc after) after No_match

let genlists_mname =
  "strl_genlists"

let g_mod name =
  Get_global (Atom name)

let g_mod_prop mname fname =
  Get_field (g_mod mname, (Atom fname))

let create_tuple elts =
  Create_block (Block_tag.Tuple, elts)

let tuple elts =
  Block (Block_tag.Tuple, elts)

let rec from_node node =
  let gen = Id.gen () in
  let cur_exit = ref (-1) in
  let new_exit () =
    cur_exit := !cur_exit + 1;
    !cur_exit
  in
  let lmod = ref {
      mod_name = None;
      mod_authors = [];
      mod_exports = [];
      mod_code = Nop;
    } in
  let mod_desc = ref Module.empty in
  let rec f = function
    | Ast_t.Module m ->
      let mod_name = ref "" in
      let export_desc : (string * int) list ref = ref [] in
      let record_desc : Module.record list ref = ref [] in

      (* attributes and declarations *)
      let attrs, fdecls =
        List.fold_left m.module_decls
          ~init:([], [])
          ~f:(fun (attrs, fdecls) decl ->
              match decl with
              | Modname_attr attr ->
                let name = attr.modname_attr_name.desc in
                mod_name := name;
                lmod := { !lmod with mod_name = Some name };
                ("module", Atom name) :: attrs, fdecls

              | Author_attr attr ->
                let name = attr.auth_attr_name.desc in
                lmod := { !lmod with mod_authors = name :: !lmod.mod_authors };
                ("author", Atom name) :: attrs, fdecls

              | Export_attr attr ->
                let sigs = List.map
                    (Seplist.values attr.export_attr_funs)
                    ~f:(fun fsig ->
                        let name = fsig.fun_sig_name.desc in
                        let arity = Int.of_string fsig.fun_sig_arity.desc in
                        export_desc := (name, arity) :: !export_desc;
                        name, arity)
                in
                let attrs = List.fold_left sigs
                    ~init:attrs
                    ~f:(fun attrs (name, arity) ->
                        lmod := { !lmod with
                                  mod_exports = (name, arity) :: !lmod.mod_exports };
                        ("export", Fun_sig (name, arity)) :: attrs)
                in
                attrs, fdecls

              | User_attr attr ->
                let values =
                  Option.value_map attr.user_attr_values
                    ~default:(tuple [])
                    ~f:(fun values ->
                        let values = List.map (extract values) ~f in
                        tuple values)
                in
                (attr.user_attr_tag.desc, values) :: attrs, fdecls

              | Record_attr attr ->
                let desc, attr = f_rec_attr attr in
                record_desc := desc :: !record_desc;
                attr :: attrs, fdecls

              | Fun_decl decl ->
                attrs, decl :: fdecls

              | _ -> failwith "notimpl")
      in

      (* add module description *)
      mod_desc := { Module.name = !mod_name;
                    exports = List.rev !export_desc;
                    records = List.rev !record_desc };
      Module.add !mod_desc;

      let funs = List.rev_map fdecls ~f:f_decl in

      (* args *)
      let attrs =
        List.fold_left (List.rev attrs)
          ~init:[]
          ~f:(fun accu (name, value) ->
              tuple [Atom name; value]:: accu)
        |> List.rev
      in
      let binds, funs =
        List.fold_left (List.rev funs)
          ~init:([], [])
          ~f:(fun (binds, funs) (name, id, decl) ->
              (id, decl) :: binds, create_tuple [Atom name; Local id] :: funs)
      in
      let attrs_block = Block (Block_tag.Tuple, List.rev attrs) in
      let funs_block = create_tuple (List.rev funs) in
      let args = [attrs_block; funs_block] in

      (* create module *)
      let fn = g_mod_prop "strl_runtime" "create_module" in
      let mname = match !lmod.mod_name with
        | Some name -> name
        | None -> failwith "module name must not be nil"
      in
      Module {
        !lmod with
        mod_code = Set_global (Atom mname, Let (binds, Apply (fn, args)))
      }

    | Case case ->
      let claus = Seplist.values case.case_clauses in
      let cases = List.map claus
          ~f:(fun clau ->
              (clau.cr_clause_ptn,
               (* guard = exp_list node_list *)
               Option.value_map clau.cr_clause_guard
                 ~default:[]
                 ~f:(fun guard ->
                     List.map (Seplist.values guard)
                       ~f:Seplist.values),
               clau.cr_clause_body)) in
      f_match case.case_exp cases

    | Call call ->
      let fname0 = call.call_fname in
      let args = List.map (Seplist.values call.call_args) ~f in
      begin match fname0.fun_name_mname with
        | Some name ->
          let mname = Get_global (f name) in
          let fobj = Get_field (mname, f fname0.fun_name_fname) in
          ev_before
            (* TODO: function name loc *)
            (Location.union call.call_open call.call_close)
            (Apply (fobj, args))
        | None ->
          begin match f fname0.fun_name_fname, args with
            | Atom "is_atom", [arg] -> Test_atom arg
            | Atom "is_binary", [arg] -> Test_binary arg
            | Atom "is_bitstring", [arg] -> Test_bitstr arg
            | Atom "is_boolean", [arg] -> Test_bool arg
            | Atom "is_float", [arg] -> Test_float arg
            | Atom "is_function", [arg] -> Test_fun1 arg
            | Atom "is_function", [arg1; arg2] -> Test_fun2 (arg1, arg2)
            | Atom "is_integer", [arg] -> Test_int arg
            | Atom "is_list", [arg] -> Test_list arg
            | Atom "is_number", [arg] -> Test_number arg
            | Atom "is_pid", [arg] -> Test_pid arg
            | Atom "is_port", [arg] -> Test_port arg
            | Atom "is_record", [arg1; arg2] -> Test_record2 (arg1, arg2)
            | Atom "is_record", [arg1; arg2; arg3] -> Test_record3 (arg1, arg2, arg3)
            | Atom "is_reference", [arg] -> Test_ref arg
            | Atom "is_tuple", [arg] -> Test_tuple arg
            | Atom "self", [] -> Self
            | Atom "spawn", [fn; args] ->
              Spawn (fn, args)
            | Atom "spawn", [mname; fname; args] ->
              Spawn (Get_field (Get_global mname, fname), args)
            | Atom "spawn", _ ->
              failwith "spawn must takes 2-3 args"
            | fname, _ ->
              let fobj =
                Get_field
                  (Get_global
                     (Atom (Option.value_exn !lmod.mod_name)), fname) in
              Apply (fobj, args)
          end
      end

    | Binexp exp ->
      begin match exp.binexp_op.desc with
        | Op_eq ->
          failwith (sprintf "notimpl binexp %s" (Ast.to_string node))

        | op ->
          let l = f exp.binexp_left in
          let r = f exp.binexp_right in
          match op with
          | Op_add -> Add (l, r)
          | Op_sub -> Sub (l, r)
          | Op_mul -> Mul (l, r)
          | Op_div -> Div (l, r)
          | Op_rem -> Rem (l, r)
          | Op_quo -> Quo (l, r)
          | Op_list_concat -> List_concat (l, r)
          | Op_list_sub -> List_sub (l, r)
          | _ -> failwith "notimpl binexp"
      end

    | Field fld ->
      let rname = fld.field_rname.desc in
      let fname = fld.field_fname.desc in
      let r = match Module.get_rec !mod_desc rname with
        | None -> failwith (sprintf "record #%s not found" rname)
        | Some r -> r
      in
      let i = match Module.get_field r fname with
        | None -> failwith (sprintf "#%s.%s not found" r.rec_name fname)
        | Some i -> i
      in
      let i' = Int (Int.to_string i) in
      begin match Option.map fld.field_exp ~f with
        | None -> i'
        | Some exp -> Get_field (exp, i')
      end

    | Update up ->
      let exp = Option.map up.update_exp ~f in
      let assocs = List.fold_right
          (Seplist.values up.update_assocs)
          ~init:[]
          ~f:(fun a accu ->
              (a.assoc_key.desc, f a.assoc_val) :: accu) in
      Update_rec {
        up_exp = exp;
        up_name = up.update_name.desc;
        up_assocs = assocs;
      }

    | Seq exps ->
      f_seq (extract exps)

    | Paren exp ->
      f exp.enc_desc

    | Var name ->
      Local name.desc

    | Atom atom ->
      begin match (Ast.text_of_atom atom).desc with
        | "true" -> Bool true
        | "false" -> Bool false
        | name -> Atom name
      end

    | Int text ->
      Int text.desc

    | Float text ->
      Float text.desc

    | String texts ->
      String (List.map texts (fun text -> text.desc)
              |> String.concat)

    | List list ->
      let head = List.fold_right (Seplist.values list.list_head)
          ~init:(create_block Block_tag.List [])
          ~f:(fun exp tail -> create_block Block_tag.List [f exp; tail])
      in
      begin match list.list_tail with
        | None -> head
        | Some tail -> List_concat (head, f tail)
      end

    | List_compr com ->
      f_list_compr com

    | Tuple exps ->
      Seplist.values exps.enc_desc
      |> List.map ~f
      |> create_block Block_tag.Tuple

    | Binary elts ->
      create_block Block_tag.Binary (List.map (Seplist.values elts.enc_desc) ~f)

    | Binary_elt elt ->
      Temp_bitstr (f_binelt elt)

    | _ -> failwith (sprintf "notimpl %s" (Ast.to_string node))

  and f_rec_attr attr =
    let f_field (field : Ast_t.type_field) =
      let name = field.ty_field_name.desc in
      let init = Option.value_map field.ty_field_init
          ~default:(Atom "undefined") ~f in
      name, tuple [
        tuple [Atom "name"; Atom name];
        tuple [Atom "init"; init]
      ]
    in
    let name = attr.rec_attr_name.desc in
    let fnames, fexps =
      Option.value_map attr.rec_attr_fields
        ~default:[]
        ~f:(fun fields -> List.map (extract fields) ~f:f_field)
      |> List.rev
      |> List.unzip
    in
    ({ Module.rec_name = name;
       rec_fields = fnames },
     ("record", 
      tuple [
        tuple [Atom "name"; Atom name];
        tuple [Atom "fields"; tuple fexps]]))

  and f_match
      (value : Ast_t.t)
      (* cases: (pattern * guard * action) list *)
      (cases : (Ast_t.t * (Ast_t.t list list) * Ast_t.t) list) =
    let mid = Id.match_ gen in
    let body =
      List.fold_right cases
        ~init:(new_exit () + List.length cases - 1, No_match)
        ~f:(fun (ptn, guard, action) (exit, next) ->
            let action = f action in
            exit - 1,
            match guard with
            | [] ->
              Catch (f_case
                       ~exit
                       ~value:(Local mid)
                       ~ptn
                       ~action,
                     [(exit, next)])
            | guard ->
              Catch (f_case
                       ~exit
                       ~value:(Local mid)
                       ~ptn
                       ~action:(f_guard guard action),
                     [(exit, next)]))
      |> Tuple2.get2
    in
    Let ([(mid, f value)], body)

  and f_match_params
      (ids : string list)
      (cases : (Ast_t.t list * Ast_t.t) list)
    : t =
    List.fold_right cases
      ~init:(new_exit () + List.length cases - 1, No_match)
      ~f:(fun (ptns, action) (exit, next) ->
          let next = List.fold_right (List.zip_exn ids ptns)
              ~init:next
              ~f:(fun (id, ptn) next ->
                  Catch (f_case
                           ~exit
                           ~value:(Local id)
                           ~ptn
                           ~action:(f action),
                         [(exit, next)]))
          in
          exit - 1, next)
    |> Tuple2.get2

  and f_case
      ~(exit : int)
      ~(value : t)
      ~(ptn : Ast_t.t)
      ~(action : t)
    : t =
    match ptn with
    | Ast_t.Nop ->
      failwith "nop"
    | Var name ->
      Let ([(name.desc, value)], action)
    | Uscore _ ->
      action
    | Int text ->
      If (Ne (value, Int text.desc), Exit exit, action)

    | Tuple ptns ->
      let ptns = Seplist.values ptns.enc_desc in
      let len = List.length ptns in
      let body =
        List.fold_right ptns
          ~init:(len - 1, action)
          ~f:(fun ptn (i, action) ->
              i - 1, f_case
                ~exit
                ~value:(Get_field (value,
                                   Int (Int.to_string i)))
                ~ptn
                ~action)
        |> Tuple2.get2
      in
      If (Not (Test_tuple value),
          Exit exit,
          If (Ne (Block_size value, Int (Int.to_string len)),
              Exit exit,
              body))

    | Binary bin ->
      let ptns = Seplist.values bin.enc_desc in
      let poss_rev = List.fold_left ptns
          ~init:[]
          ~f:(fun accu ptn ->
              match ptn with
              | Ast_t.Binary_elt ptn ->
                begin match accu with
                  | [] ->
                    Option.value_map ptn.bin_elt_size
                      ~default:[Int "0"]
                      ~f:(fun size -> [f size])
                  | pre :: _ ->
                    Option.value_map ptn.bin_elt_size
                      ~default:(pre :: accu)
                      ~f:(fun size -> Add (pre, f size) :: accu)
                end
              | _ -> failwith "error")
      in

      List.fold_right ptns
        ~init:(new_exit () + List.length ptns - 1, poss_rev, action)
        ~f:(fun ptn (exit, poss, action') ->
            match ptn with
            | Binary_elt ptn' ->
              let exp = f_bin_case
                  ~exit
                  ~value
                  ~ptn:ptn'
                  ~pos:(List.hd_exn poss)
                  ~action:action'
              in
              (exit - 1, List.tl_exn poss, exp)
            | _ -> failwith "not binary elt")
      |> Tuple3.get3

    | Binary_elt elt ->
      failwith "error"

    | _ -> failwith (sprintf "notimpl case %s" (Ast.to_string ptn))

  and f_bin_case
      ~(exit : int)
      ~(value : t)
      ~(ptn : Ast_t.binary_elt)
      ~(pos : t)
      ~(action : t)
    : t =
    let bits = f_binelt ptn in
    let l = Get_bitstr (value, bits.spec, pos) in
    match ptn.bin_elt_val with
    | Var var ->
      Let ([(var.desc, l)], action)
    | _ ->
      let r = Get_bitstr (bits.value, bits.spec, pos) in
      If (Ne (l, r), Exit exit, action)

  and f_binelt elt : bitstr =
    let module P = Bitstr.Parser in
    let value = f elt.bin_elt_val in
    match elt.bin_elt_size, elt.bin_elt_type with
    | None, None ->
      (* TODO *)
      Bitstr.create
        ~value
        ~size:(Int "8")
        ~ty:`Int
        ~sign:`Unsigned
        ~endian:`Native
        ~unit:(Some 8)
    | Some size, None ->
      Bitstr.create
        ~value
        ~size:(f size)
        ~ty:`Int
        ~sign:`Unsigned
        ~endian:`Big
        ~unit:(Some 8)
    | None, Some ty ->
      Bitstr.create
        ~value
        ~size:(Int (Int.to_string (P.size ty.desc)))
        ~ty:(P.ty ty.desc)
        ~sign:(P.sign ty.desc)
        ~endian:(P.endian ty.desc)
        ~unit:(P.unit ty.desc)
    | Some size, Some ty ->
      Bitstr.create
        ~value
        ~size:(f size)
        ~ty:(P.ty ty.desc)
        ~sign:(P.sign ty.desc)
        ~endian:(P.endian ty.desc)
        ~unit:(P.unit ty.desc)

  and f_guard (cond : Ast_t.t list list) (action : t) =
    List.fold_right cond
      ~init:action
      ~f:(fun cond next ->
          let exn = Id.match_ gen in 
          let cond = match cond with
            | [exp] -> f exp
            | exp :: exps ->
              List.fold_right exps
                ~init:(f exp)
                ~f:(fun exp accu -> And (f exp, accu))
            | _ -> failwith "must not be executed"
          in
          let try_ = Try (cond, exn, Bool false) in
          Or (try_, next))

  and f_seq (exps : Ast_t.t list) =
    match exps with
    | [] -> Nop
    | exp :: exps ->
      List.fold_right exps
        ~init:(f exp)
        ~f:(fun exp next ->
            match exp with
            | Binexp bin ->
              begin match bin.binexp_op.desc with
                | Op_eq ->
                  let exit = new_exit () in
                  let rexp = f bin.binexp_right in
                  let var = Id.match_ gen in
                  Let ([(var, rexp)],
                       f_case
                         ~exit
                         ~value:rexp
                         ~ptn:bin.binexp_left
                         ~action:(Local var))
                | _ -> seq (f exp) next
              end
            | _ -> seq (f exp) next)

  and f_list_compr (com : Ast_t.compr) =
    let cgens, conds = List.fold_right (extract com.compr_quals)
        ~init:([], [])
        ~f:(fun exp (cgens, conds) ->
            match exp with
            | Binexp bin ->
              begin match bin.binexp_op.desc with
                | Op_eq -> failwith "bad filter"
                | _ -> cgens, f exp :: conds
              end
            | List_compr_gen cgen ->
              begin match cgen.gen_exp with
                | List exp ->
                  Option.iter exp.list_tail ~f:(fun _ ->
                      failwith "bad generator, list tail");
                  (cgen.gen_ptn, f cgen.gen_exp) :: cgens, conds
                | Var _ ->
                  (cgen.gen_ptn, f cgen.gen_exp) :: cgens, conds
                | _ -> failwith "bad generator, not list"
              end
            | Binary_compr_gen _ ->
              failwith "bad generator, binary generator"
            | _ -> failwith "bad filter")
    in

    (* create generator *)
    let gen_fun = g_mod_prop genlists_mname "create" in
    let next_fun = g_mod_prop genlists_mname "next" in
    let add_fun = g_mod_prop genlists_mname "add" in
    let collect_fun = g_mod_prop genlists_mname "collect" in

    let gen_exps = List.map cgens ~f:(fun (_, exp) -> exp) in
    let l_gen = Apply (gen_fun, [create_tuple gen_exps]) in
    let gen_var = Id.next gen "*listgen*" in

    (* body *)
    let l_body = Apply (add_fun, [Local gen_var; f com.compr_exp]) in

    (* filter *)
    let next_var = Id.next gen "*listnext*" in
    let l_cond =
      List.fold_right conds
        ~init:l_body
        ~f:(fun cond action -> If (cond, action, Bool false))
    in
    let l_filter =
      List.fold_right cgens
        ~init:(List.length cgens - 1, l_cond)
        ~f:(fun (ptn, _) (i, action) ->
            let exit = new_exit () in
            let case =
              f_case
                ~exit
                ~value:(Get_field (Local next_var, Int (Int.to_string i)) )
                ~ptn
                ~action in
            i - 1, Catch (case, [(exit, Nop)]))
      |> Tuple2.get2
    in

    (* loop *)
    let get_next = Apply (next_fun, [Local gen_var]) in
    let l_loop_cond = Test_nonnil (Local next_var) in
    let l_loop_body =
      Let ([(next_var, get_next)],
           Loop (l_loop_cond,
                 Seq (l_filter,
                      Set_local (next_var, get_next))))
    in

    Let ([(gen_var, l_gen)],
         Seq (l_loop_body, Apply (collect_fun, [Local gen_var])))

  and f_decl decl : (string * Id.t * t) =
    let claus = Seplist.values decl.fun_decl_body in
    let head = List.hd_exn claus in
    let name = Option.value_exn head.fun_clause_name in

    (* parameters *)
    let params = List.map
        (Seplist.values head.fun_clause_ptns)
        ~f:(fun _ -> Id.param gen) in

    (* clauses *)
    (* if all parameter patterns are variables, pattern matching can be omitted *)
    let all_vars (clau : Ast_t.fun_clause) =
      let ptns = Seplist.values clau.fun_clause_ptns in
      List.for_all ptns ~f:(function
          | Ast_t.Var _ -> true
          | _ -> false) 
    in
    let params, body = match params, claus with
      | [], [clau] ->
        params, f clau.fun_clause_body
      | [], _ -> failwith "clauses > 0"
      | _, [clau] when all_vars clau ->
        let ptns = Seplist.values clau.fun_clause_ptns in
        let params = List.map ptns ~f:(function
            | Ast_t.Var name -> name.desc
            | _ -> failwith "error") 
        in
        params, f clau.fun_clause_body
      | _, _ ->
        let cases = List.map claus ~f:(fun clau ->
            Seplist.values clau.fun_clause_ptns, clau.fun_clause_body)
        in
        params, f_match_params params cases
    in

    (* position *)
    let fst = List.hd_exn claus in
    let start = Option.value_map fst.fun_clause_name
        ~default:fst.fun_clause_open.start
        ~f:(fun name -> name.loc.start) in
    let ev = ev_fun
        (Location.create start decl.fun_decl_dot.end_)
        (ev_before
           name.loc
           (Fun (Some name.desc, params, body)))
    in
    (name.desc, Id.next gen name.desc, ev)

  and seq exp1 exp2 = 
    match exp2 with
    | Nop -> exp1
    | _ -> Seq (exp1, exp2)

  and create_block tag elts =
    let rec check lit elt =
      match lit, elt with
      | _, Int _ -> lit, elt
      | _, Temp_block (tag, []) -> true, Block (tag, [])
      | _, Temp_block (tag, elts) ->
        begin match check_list lit elts with
          | true, elts -> true, Block (tag, elts)
          | false, elts -> false, Create_block (tag, elts)
        end
      | _, Temp_bitstr bits ->
        begin match check lit bits.value with
          | true, _ -> true, Bitstr bits
          | false, _ -> false, Create_bitstr bits
        end
      | true, Bitstr _ -> true, elt
      | false, Bitstr bits -> false, Create_bitstr bits
      | true, Block _ -> true, elt
      | false, Block (tag, elts) -> false, Create_block (tag, elts)
      | _ -> false, elt

    and check_list lit elts =
      List.fold_right elts
        ~init:(lit, [])
        ~f:(fun elt (lit, accu) ->
            let lit, elt = check lit elt in
            lit, elt :: accu)
    in

    match check_list true elts with
    | true, elts -> Block (tag, elts)
    | false, elts -> Create_block (tag, elts)

  in
  f node

