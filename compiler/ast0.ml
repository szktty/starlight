open Core
open Common
open Located
open Ast_t

let text_of_atom atom =
  match atom with
  | `Unenclosed text
  | `Enclosed text -> text

let enclose open_ desc close =
  { enc_open = open_;
    enc_desc = desc;
    enc_close = close }

let create desc =
  match desc with
  | List list ->
    Located.with_range list.list_open list.list_close desc
  | _ -> failwith "notimpl"

let simple_fun_name name =
  { fun_name_mname = None;
    fun_name_colon = None;
    fun_name_fname = name; }

let rec start_pos node =
  let open Located in
  let open Location in

  let of_loc loc =
    loc.start
  in

  let of_text text =
    text.loc.start
  in

  let of_fun_clause clause =
    match clause.fun_clause_name with
    | Some name -> of_text name
    | None -> clause.fun_clause_open.start
  in

  match node with
  | Module m ->
    begin match m.module_decls with
      | [] -> m.module_eof.start
      | hd :: _ -> start_pos hd
    end
  | Modname_attr attr -> of_text attr.modname_attr_tag
  | Author_attr attr -> of_text attr.auth_attr_tag
  | Compile_attr attr -> of_text attr.compile_attr_tag
  | Export_attr attr -> of_text attr.export_attr_tag
  | Export_type_attr attr -> of_text attr.export_attr_tag
  | File_attr attr -> of_text attr.file_attr_tag
  | Import_attr attr -> of_text attr.import_attr_tag
  | Include_attr attr -> of_text attr.include_attr_tag
  | Inclib_attr attr -> of_text attr.inclib_attr_tag
  | Spec_attr attr -> of_text attr.spec_attr_tag
  | Type_attr attr -> of_text attr.type_attr_tag
  | Onload_attr attr -> of_text attr.onload_attr_tag
  | Opaque_attr attr -> of_text attr.type_attr_tag
  | Opt_cbs_attr attr -> of_text attr.opt_attr_tag
  | Define_attr attr -> of_text attr.def_attr_tag
  | Depr_attr attr -> of_text attr.depr_attr_tag
  | Behav_attr attr -> of_text attr.behav_attr_tag
  | Callback_attr attr -> of_text attr.cb_attr_tag
  | Record_attr attr -> of_text attr.rec_attr_tag
  | Flow_macro_attr attr -> of_text attr.flow_macro_attr_tag
  | Flow_attr attr -> of_text attr.flow_attr_tag
  | User_attr attr -> of_text attr.user_attr_tag
  | Vsn_attr attr -> of_text attr.vsn_attr_tag
  | Fun_decl decl -> of_fun_clause @@ Seplist.hd_exn decl.fun_decl_body
  | Catch (tok, _) -> tok.start
  | Block seq -> start_pos seq.enc_desc
  | If if_ -> if_.if_begin.start
  | Case case -> case.case_begin.start
  | Recv recv -> recv.recv_begin.start
  | Try try_ -> try_.try_begin.start
  | Anon_fun f -> f.anon_fun_begin.start
  | Module_fun f -> f.module_fun_prefix.start
  | Unexp (op, _) -> of_loc op.loc
  | Binexp exp -> start_pos exp.binexp_left
  | Call call ->
    start_pos @@ Option.value
      call.call_fname.fun_name_mname
      ~default:call.call_fname.fun_name_fname
  | Seq exps -> start_pos (Seplist.hd_exn exps)
  | Paren exp -> exp.enc_open.start
  | Atom atom -> of_text @@ text_of_atom atom
  | String values -> of_text (List.hd_exn values)
  | List list -> list.list_open.start
  | Binary exp -> exp.enc_open.start
  | Binary_elt elt -> start_pos elt.bin_elt_val
  | Tuple exp -> exp.enc_open.start
  | Field field ->
    begin match field.field_exp with
      | None -> field.field_sharp.start
      | Some node -> start_pos node
    end
  | Update update ->
    begin match update.update_exp with
      | None -> update.update_sharp.start
      | Some node -> start_pos node
    end
  | List_compr compr
  | Binary_compr compr ->
    compr.compr_open.start
  | List_compr_gen gen -> start_pos gen.gen_ptn
  | Binary_compr_gen gen -> start_pos gen.bin_gen_ptn
  | Map map -> map.map_nsign.start
  | Macro m -> m.macro_q.start
  | Uscore value
  | Var value
  | Char value
  | Int value
  | Float value -> of_text value
  | Nop -> Position.zero

let rec end_pos node =
  let open Located in
  let open Location in

  let of_loc loc = loc.end_ in

  let of_text text =
    of_loc text.loc
  in

  let of_fun_clause clause =
    match clause.fun_clause_name with
    | Some name -> of_text name
    | None -> clause.fun_clause_open.end_
  in

  match node with
  | Module m ->
    begin match m.module_decls with
      | [] -> m.module_eof.end_
      | hd :: _ -> end_pos hd
    end
  | Modname_attr attr -> attr.modname_attr_dot.end_
  | Author_attr attr -> attr.auth_attr_dot.end_
  | Compile_attr attr -> attr.compile_attr_dot.end_
  | Export_attr attr -> attr.export_attr_dot.end_
  | Export_type_attr attr -> attr.export_attr_dot.end_
  | File_attr attr -> attr.file_attr_dot.end_
  | Import_attr attr -> attr.import_attr_dot.end_
  | Include_attr attr -> attr.include_attr_dot.end_
  | Inclib_attr attr -> attr.inclib_attr_dot.end_
  | Spec_attr attr -> attr.spec_attr_dot.end_
  | Type_attr attr -> attr.type_attr_dot.end_
  | Onload_attr attr -> attr.onload_attr_dot.end_
  | Opaque_attr attr -> attr.type_attr_dot.end_
  | Opt_cbs_attr attr -> attr.opt_attr_dot.end_
  | Define_attr attr -> attr.def_attr_dot.end_
  | Depr_attr attr -> attr.depr_attr_dot.end_
  | Behav_attr attr -> attr.behav_attr_dot.end_
  | Callback_attr attr -> attr.cb_attr_dot.end_
  | Record_attr attr -> attr.rec_attr_dot.end_
  | Flow_macro_attr attr -> attr.flow_macro_attr_dot.end_
  | Flow_attr attr -> attr.flow_attr_dot.end_
  | User_attr attr -> attr.user_attr_dot.end_
  | Vsn_attr attr -> attr.vsn_attr_dot.end_
  | Fun_decl decl -> of_fun_clause @@ Seplist.hd_exn decl.fun_decl_body
  | Catch (tok, _) -> tok.start
  | Block seq -> end_pos seq.enc_desc
  | If if_ -> if_.if_begin.start
  | Case case -> case.case_begin.start
  | Recv recv -> recv.recv_begin.start
  | Try try_ -> try_.try_begin.start
  | Anon_fun f -> f.anon_fun_begin.start
  | Module_fun f -> f.module_fun_prefix.start
  | Unexp (op, _) -> of_loc op.loc
  | Binexp exp -> start_pos exp.binexp_left
  | Call call ->
    start_pos @@ Option.value
      call.call_fname.fun_name_mname
      ~default:call.call_fname.fun_name_fname
  | Seq exps -> end_pos (Seplist.hd_exn exps)
  | Paren exp -> exp.enc_open.start
  | Atom atom -> of_text @@ text_of_atom atom
  | String values -> of_text (List.hd_exn values)
  | List list -> list.list_open.start
  | Binary exp -> exp.enc_open.start
  | Binary_elt elt -> start_pos elt.bin_elt_val
  | Tuple exp -> exp.enc_open.start
  | Field field ->
    begin match field.field_exp with
      | None -> field.field_sharp.start
      | Some node -> start_pos node
    end
  | Update update ->
    begin match update.update_exp with
      | None -> update.update_sharp.start
      | Some node -> start_pos node
    end
  | List_compr compr
  | Binary_compr compr ->
    compr.compr_open.start
  | List_compr_gen gen -> start_pos gen.gen_ptn
  | Binary_compr_gen gen -> start_pos gen.bin_gen_ptn
  | Map map -> map.map_nsign.start
  | Macro m -> m.macro_q.start
  | Uscore value
  | Var value
  | Char value
  | Int value
  | Float value -> of_text value
  | Nop -> Position.zero

let loc node =
  Location.create (start_pos node) (end_pos node)
