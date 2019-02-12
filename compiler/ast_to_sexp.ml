open Core
open Common
open Located
open Ast_t
open Ast0

let rec to_sexp = function
  | Nop ->
    Sexp.Atom "nop"
  | Module m ->
    Sexp.tagged "module" (to_sexp_list m.module_decls)
  | Modname_attr attr ->
    Sexp.tagged "module_attr" [Sexp.Atom attr.modname_attr_name.desc]
  | Author_attr attr ->
    Sexp.tagged "author_attr" [Sexp.Atom attr.auth_attr_name.desc]
  | Behav_attr attr ->
    Sexp.tagged "behav_attr" [Sexp.Atom attr.behav_attr_name.desc]
  | Compile_attr attr ->
    Sexp.tagged "compile_attr" [to_sexp attr.compile_attr_value]
  | Define_attr attr ->
    let name = attr.def_attr_name in
    Sexp.tagged "define_attr"
      [Sexp.Atom name.def_name.desc;
       Sexp.List
         (Option.value_map name.def_args
            ~default:[]
            ~f:(fun args ->
                extract args.enc_desc
                |> List.map ~f:(fun arg -> Sexp.Atom arg.desc)));
       to_sexp attr.def_attr_value]
  | Depr_attr attr ->
    Sexp.tagged "depr_attr" [to_sexp attr.depr_attr_list]
  | Export_attr attr ->
    Sexp.tagged "export_attr"
      (List.map (Seplist.values attr.export_attr_funs) ~f:fun_sig_to_sexp)
  | File_attr attr ->
    Sexp.tagged "file_attr" [
      Sexp.Atom attr.file_attr_path.desc;
      Sexp.Atom attr.file_attr_line.desc;
    ]
  | Flow_macro_attr attr ->
    let tag = match attr.flow_macro_attr_tag_type with
      | `Undef -> "undef_attr"
      | `Ifdef -> "ifdef_attr"
      | `Ifndef -> "ifndef_attr"
    in
    Sexp.tagged tag [Sexp.Atom attr.flow_macro_attr_macro.desc]
  | Flow_attr attr ->
    Sexp.Atom (match attr.flow_attr_tag_type with
        | `Else -> "else_attr"
        | `Endif -> "endif_attr")
  | Import_attr attr ->
    Sexp.tagged "import_attr" [
      spair "module" (Sexp.Atom attr.import_attr_module.desc);
      Sexp.tagged "import"
        (List.map (extract attr.import_attr_funs) ~f:fun_sig_to_sexp)
    ]
  | Include_attr attr ->
    Sexp.tagged "include_attr" [Sexp.Atom attr.include_attr_file.desc]
  | Record_attr attr ->
    let fields = match attr.rec_attr_fields with
      | None -> []
      | Some fields ->
        List.map (extract fields)
          ~f:(fun field ->
              let name = field.ty_field_name.desc in
              let init = Option.value_map field.ty_field_init
                  ~default:(Sexp.Atom "<none>")
                  ~f:to_sexp 
              in
              Sexp.List [Sexp.Atom name; init])
    in
    Sexp.tagged "record_attr"
      [Sexp.Atom attr.rec_attr_name.desc; Sexp.List fields]
  | Spec_attr attr ->
    Sexp.tagged "spec_attr" (concat_opts [
        Option.map attr.spec_attr_mname
          ~f:(fun (name, _) -> Sexp.Atom name.desc);
        Some (Sexp.Atom attr.spec_attr_fname.desc);
        Some (spec_claus_to_sexp (extract attr.spec_attr_clauses));
      ])
  | Fun_decl decl ->
    Sexp.tagged "fun_decl" (sexp_fun_body decl.fun_decl_body)
  | Case case ->
    Sexp.tagged "case" [to_sexp case.case_exp;
                        (cr_claus_to_sexp
                           (Seplist.values case.case_clauses))]
  | Try try_ ->
    spair "try" (sconcat [
        Some (to_sexp try_.try_exps);
        Option.map try_.try_clauses
          ~f:(fun claus ->
              spair "case" (cr_claus_to_sexp (extract claus)));
        Option.map try_.try_catch.try_catch_clauses
          ~f:(fun claus ->
              spair "try-catch" (try_claus_to_sexp (extract claus)))
      ])
  | Recv recv ->
    Sexp.List [Sexp.Atom "receive";
               cr_claus_to_sexp (Seplist.values recv.recv_clauses);
               Sexp.Atom "after";
               Option.value_map recv.recv_after
                 ~default:(Sexp.Atom "nop")
                 ~f:(fun after ->
                     Sexp.List [to_sexp after.recv_after_timer;
                                to_sexp after.recv_after_body])]
  | Call call ->
    Sexp.tagged "call" [
      let fname = call.call_fname in
      Sexp.tagged "fun_name" [
        begin match fname.fun_name_mname with
          | None -> to_sexp fname.fun_name_fname
          | Some mname ->
            Sexp.List [
              to_sexp mname;
              to_sexp fname.fun_name_fname]
        end;
        Sexp.tagged "args" (to_sexp_seplist call.call_args)
      ]
    ] 
  | Binexp exp ->
    Sexp.List [op_to_sexp exp.binexp_op.desc;
               to_sexp exp.binexp_left;
               to_sexp exp.binexp_right]
  | Seq exps ->
    Sexp.List (to_sexp_seplist exps)
  | Paren exp ->
    to_sexp exp.enc_desc
  | Var name ->
    Sexp.Atom ("$" ^ name.desc)
  | Uscore _ ->
    Sexp.Atom "_"
  | Atom atom ->
    let text = text_of_atom atom in
    Sexp.Atom (text.desc ^ "!")
  | Int text ->
    Sexp.tagged "int" [Sexp.Atom text.desc]
  | Float text ->
    Sexp.tagged "float" [Sexp.Atom text.desc]
  | String texts ->
    Sexp.tagged "string" (List.map texts ~f:(fun text -> Sexp.Atom text.desc))
  | List list ->
    let head = to_sexp_seplist list.list_head in
    let tail = match list.list_tail with
      | None -> []
      | Some exp -> [to_sexp exp]
    in
    Sexp.tagged "list" (head @ tail)
  | List_compr com ->
    Sexp.tagged "list_compr" [to_sexp com.compr_exp;
                              Sexp.List (to_sexp_seplist com.compr_quals)]
  | List_compr_gen gen ->
    Sexp.tagged "list_compr_gen" [to_sexp gen.gen_ptn;
                                  to_sexp gen.gen_exp]
  | Binary bin ->
    let exps = Seplist.values bin.enc_desc in
    Sexp.tagged "binary" (List.map exps ~f:to_sexp)
  | Binary_elt elt ->
    Sexp.tagged "binelt" [to_sexp elt.bin_elt_val;
                          Option.value_map elt.bin_elt_size
                            ~default:(Sexp.Atom "-")
                            ~f:to_sexp;
                          Option.value_map elt.bin_elt_type
                            ~default:(Sexp.Atom "-")
                            ~f:(fun ty -> Sexp.Atom ty.desc)]
  | Tuple exps ->
    Sexp.tagged "tuple" (to_sexp_seplist exps.enc_desc)
  | Field field ->
    let exp =
      Option.value_map field.field_exp
        ~default:(Sexp.Atom "<none>")
        ~f:to_sexp in
    let rname = field.field_rname.desc in
    let fname = field.field_fname.desc in
    Sexp.tagged "field" [exp; Sexp.Atom rname; Sexp.Atom fname]
  | Update up ->
    let exp =
      Option.value_map up.update_exp
        ~default:(Sexp.Atom "<none>")
        ~f:to_sexp in
    let name = up.update_name.desc in
    let assocs =
      List.map (extract up.update_assocs)
        ~f:(fun assoc ->
            Sexp.List [Sexp.Atom (assoc.assoc_key.desc);
                       to_sexp assoc.assoc_val]) in
    Sexp.tagged "update" [exp; Sexp.Atom name; Sexp.List assocs]
  | Anon_fun fn ->
    Sexp.tagged "anon_fun" (sexp_fun_body fn.anon_fun_body)
  | _ -> Sexp.Atom "<notimpl>"

and to_sexp_list list =
  List.map list ~f:to_sexp

and to_sexp_seplist list =
  Seplist.values list
  |> List.map ~f:to_sexp

and fun_sig_to_sexp fsig =
  Sexp.Atom (sprintf "%s/%s" fsig.fun_sig_name.desc fsig.fun_sig_arity.desc)

and sexp_fun_body body =
  Seplist.values body
  |> List.fold_left
    ~init:[]
    ~f:(fun accu clau ->
        let name = Option.value_map clau.fun_clause_name
            ~default:"?fun_clause_name"
            ~f:(fun text -> text.desc)
        in
        let ptns =
          List.map (Seplist.values clau.fun_clause_ptns)
            ~f:to_sexp in
        let guard = guard_opt_to_sexp clau.fun_clause_guard in
        let exps = to_sexp clau.fun_clause_body in
        Sexp.tagged "fun_clause" [
          Sexp.Atom name;
          Sexp.List ptns;
          Sexp.List guard;
          exps] :: accu)
  |> List.rev

and spec_claus_to_sexp claus =
  Sexp.List
    (List.map claus
       ~f:(fun clau ->
           let args =
             Option.value_map clau.spec_clause_args
               ~default:[]
               ~f:(fun args ->
                   let args = List.map (extract args) ~f:type_to_sexp in
                   [Sexp.List args]) in
           let ret = type_to_sexp clau.spec_clause_return in
           let guard =
             Option.map clau.spec_clause_guard
               ~f:(fun (_, constrs) ->
                   spair "guard" ( spec_guard_to_sexp constrs)) in
           sconcat [Some (Sexp.tagged "args" args);
                    Some (Sexp.tagged "return" [ret]);
                    guard]))

and spec_guard_to_sexp constrs =
  List.map (extract constrs)
    ~f:type_constr_to_sexp
  |> slist

and type_constr_to_sexp constr =
  slist [Sexp.Atom constr.ty_constr_name.desc;
         Option.value_exn constr.ty_constr_type
         |> Tuple2.get2
         |> type_to_sexp]

and type_to_sexp ty =
  let tag, value = 
    match ty with
    | Ty_paren ty -> "paren", type_to_sexp ty.enc_desc
    | Ty_atom name -> "atom", Sexp.Atom (text_of_atom name).desc
    | Ty_int text -> "int", Sexp.Atom text.desc
    | Ty_range ty ->
      "range", Sexp.List [Sexp.Atom ty.ty_range_start.desc;
                          Sexp.Atom ty.ty_range_end.desc]
    | Ty_nil _ -> "nil", Sexp.List []
    | Ty_named ty ->
      let tag = Option.value_map ty.ty_named_module
          ~default:""
          ~f:(fun name -> name.desc ^ ":") ^
                ty.ty_named_name.desc
      in
      let args = Option.value_map ty.ty_named_args
          ~default:[]
          ~f:(fun args ->
              List.map (extract args)
                ~f:type_to_sexp)
      in
      tag, Sexp.List args
    | Ty_bits ty ->
      let open Buffer in
      let buf = create 16 in
      let add_text text = add_string buf text.desc in
      add_string buf "<<";
      begin match ty.ty_bits_start_uscore, ty.ty_bits_start_bits with
        | Some uscore, Some bits ->
          add_string buf (sprintf "%s:%s" uscore.desc bits.desc)
        | _ -> ()
      end;
      Option.iter ty.ty_bits_comma ~f:(fun _ -> add_string buf ",");
      Option.iter ty.ty_bits_cont_uscore1 ~f:add_text;
      Option.iter ty.ty_bits_cont_colon ~f:(fun _ -> add_string buf ":");
      Option.iter ty.ty_bits_cont_uscore2 ~f:add_text;
      Option.iter ty.ty_bits_cont_mul ~f:(fun _ -> add_string buf "*");
      Option.iter ty.ty_bits_cont_bits ~f:add_text;
      add_string buf ">>";
      "bits", Sexp.Atom (contents buf)
    | Ty_list ty -> "list", type_to_sexp ty.enc_desc
    | Ty_tuple ty ->
      "tuple",
      Sexp.List (Option.value_map ty.enc_desc
                   ~default:[]
                   ~f:(fun nodes ->
                       extract nodes
                       |> List.map ~f:type_to_sexp))
    | Ty_fun ty ->
      "fun",
      Option.value_map ty.ty_fun_body
        ~default:(Sexp.List [])
        ~f:type_fun_body_to_sexp
    | Ty_map ty ->
      "map",
      Option.value_map ty.ty_map_pairs
        ~default:[]
        ~f:(fun pairs ->
            List.map (extract pairs)
              ~f:(fun pair ->
                  slist [type_to_sexp pair.ty_pair_left;
                         type_to_sexp pair.ty_pair_right ]))
      |> slist
    | Ty_record ty ->
      let field_to_sexp field =
        sconcat [Some (spair "name" (Sexp.Atom field.ty_field_name.desc));
                 Option.map field.ty_field_init
                   ~f:(fun init -> spair "init" (to_sexp init))]
      in
      "record",
      slist [spair "name" (Sexp.Atom ty.ty_rec_name.desc);
             spair "fields"
               ((Option.value_map ty.ty_rec_fields
                   ~default:[]
                   ~f:(fun fields ->
                       List.map (extract fields) ~f:field_to_sexp))
                |> slist)]
    | Ty_union ty ->
      "union",
      slist [type_to_sexp ty.ty_union_left;
             type_to_sexp ty.ty_union_right]
    | Ty_constr constr ->
      "constr", sconcat [
        Some (satom constr.ty_constr_name.desc);
        Option.map constr.ty_constr_type
          ~f:(fun (_, ty) -> type_to_sexp ty)]
    | Ty_macro (_, text) -> "macro", Sexp.Atom text.desc
  in
  spair ("type-" ^ tag) value

and type_fun_body_to_sexp body =
  sconcat [
    Option.map body.ty_fun_body_name
      ~f:(fun name -> Sexp.Atom name.desc);
    begin match body.ty_fun_body_args with
      | `None -> None
      | `Dot _ -> Some (spair "args" (Sexp.Atom "..."))
      | `Types args ->
        Some (spair "args"
                (Sexp.List (List.map (extract args) ~f:type_to_sexp)))
    end;
    Some (spair "return" (type_to_sexp body.ty_fun_body_type))
  ]

and cr_claus_to_sexp claus =
  Sexp.List
    (List.map claus
       ~f:(fun clau ->
           Sexp.List [to_sexp clau.cr_clause_ptn;
                      Sexp.List (guard_opt_to_sexp clau.cr_clause_guard);
                      to_sexp clau.cr_clause_body]))

and try_claus_to_sexp claus =
  slist
    (List.map claus
       ~f:(fun clau ->
           sconcat [Some (spair "class" (to_sexp clau.try_clause_cls));
                    Option.map clau.try_clause_exn
                      ~f:(fun (_, exp) -> spair "exn" (to_sexp exp));
                    Option.map clau.try_clause_stack
                      ~f:(fun (_, exp) -> spair "stack" (to_sexp exp));
                    Option.map clau.try_clause_guard
                      ~f:(fun (_, ptn) -> guard_to_sexp ptn);
                    Some (to_sexp clau.try_clause_body)]))

and guard_opt_to_sexp guard_opt =
  Option.value_map guard_opt
    ~default:[]
    ~f:(fun guard -> [guard_to_sexp guard])

and guard_to_sexp guard =
  match Seplist.values guard with
  | [] -> failwith "must not be empty"
  | exps :: exps_tl ->
    let f_and exps = 
      exps_to_sexp (Seplist.values exps)
        ~f:(fun a b -> Sexp.tagged "and" [a; b])
    in
    let exp = List.fold_right exps_tl
        ~init:(f_and exps)
        ~f:(fun exps accu ->
            Sexp.tagged "or" [f_and exps; accu])
    in
    spair "guard" exp

and exps_to_sexp (exps : t list) ~f =
  match exps with
  | [] -> failwith "must not be empty"
  | exp :: exps ->
    List.fold_right exps
      ~init:(to_sexp exp)
      ~f:(fun exp accu ->
          f (to_sexp exp) accu)

and op_to_sexp op =
  Sexp.Atom (match op with
      | Op_pos -> "pos"
      | Op_neg -> "neg"
      | Op_not -> "not"
      | Op_lnot -> "lnot"
      | Op_eq -> "="
      | Op_ep -> "!"
      | Op_eqq -> "=="
      | Op_ne -> "/="
      | Op_le -> "=<"
      | Op_lt -> "<"
      | Op_ge -> ">="
      | Op_gt -> ">"
      | Op_xeq -> "=:="
      | Op_xne -> "=/="
      | Op_list_concat -> "++"
      | Op_list_sub -> "--"
      | Op_add -> "+"
      | Op_sub -> "-"
      | Op_mul -> "*"
      | Op_div -> "/"
      | Op_quo -> "div"
      | Op_rem -> "rem"
      | Op_and -> "and"
      | Op_andalso -> "andalso"
      | Op_or -> "or"
      | Op_orelse -> "orelse"
      | Op_xor -> "xor"
      | Op_sand -> "andalso"
      | Op_sor -> "orelse"
      | Op_land -> "band"
      | Op_lor -> "bor"
      | Op_lxor -> "bxor"
      | Op_lshift -> "bsl"
      | Op_rshift -> "bsr")

let to_string node =
  Sexp.to_string_hum ~indent:4 (to_sexp node)
