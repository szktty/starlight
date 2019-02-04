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
    Sexp.tagged "module-attr" [Sexp.Atom attr.modname_attr_name.desc]
  | Author_attr attr ->
    Sexp.tagged "author-attr" [Sexp.Atom attr.auth_attr_name.desc]
  | Behav_attr attr ->
    Sexp.tagged "behav-attr" [Sexp.Atom attr.behav_attr_name.desc]
  | Export_attr attr ->
    Sexp.tagged "export-attr"
      (List.map (Seplist.values attr.export_attr_funs)
         ~f:(fun fsig -> Sexp.List [Sexp.Atom fsig.fun_sig_name.desc;
                                    Sexp.Atom fsig.fun_sig_arity.desc]))
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
    Sexp.tagged "record-attr"
      [Sexp.Atom attr.rec_attr_name.desc; Sexp.List fields]
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
