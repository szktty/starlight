(* https://github.com/ignatov/intellij-erlang/blob/mter/grammars/erlang.bnf *)

type token = Location.t

type 'a node_list = ('a, token) Seplist.t

type text = string Located.t

type text_list = (text, token) Seplist.t

type 'a enclosed_lit = [`Unenclosed of 'a | `Enclosed of 'a]

type atom = text enclosed_lit

type 'a enclosed = {
  enc_open : token;
  enc_desc : 'a;
  enc_close : token;
}

type op = op_desc Located.t

and op_desc =
  | Op_pos              (* "+" *)
  | Op_neg              (* "-" *)
  | Op_not              (* "not" *)
  | Op_lnot             (* "bnot" *)
  | Op_eq               (* "=" *)
  | Op_ep               (* "!" *)
  | Op_eqq              (* "==" *)
  | Op_ne               (* "/=" *)
  | Op_le               (* "=<" *)
  | Op_lt               (* "<" *)
  | Op_ge               (* ">=" *)
  | Op_gt               (* ">" *)
  | Op_xeq              (* "=:=" *)
  | Op_xne              (* "=/=" *)
  | Op_list_concat      (* "++" *)
  | Op_list_sub         (* "--" *)
  | Op_add              (* "+" *)
  | Op_sub              (* "-" *)
  | Op_mul              (* "*" *)
  | Op_div              (* "/" *)
  | Op_quo              (* "div" *)
  | Op_rem              (* "rem" *)
  | Op_and              (* "and" *)
  | Op_andalso          (* "andalso" *)
  | Op_or               (* "or" *)
  | Op_orelse           (* "orelse" *)
  | Op_xor              (* "xor" *)
  | Op_sand             (* "andalso" *)
  | Op_sor              (* "orelse" *)
  | Op_land             (* "band" *)
  | Op_lor              (* "bor" *)
  | Op_lxor             (* "bxor" *)
  | Op_lshift           (* "bsl" *)
  | Op_rshift           (* "bsr" *)

type t =
  | Nop (* internal use *)
  | Module of module_
  | Modname_attr of modname_attr
  | Author_attr of author_attr
  | Compile_attr of compile_attr
  | Export_attr of export_attr
  | Export_type_attr of export_attr
  | Import_attr of import_attr
  | Include_attr of include_attr
  | Inclib_attr of inclib_attr
  | Spec_attr of spec_attr
  | Type_attr of type_attr
  | Onload_attr of onload_attr
  | Opaque_attr of type_attr
  | Opt_cbs_attr of opt_cbs_attr
  | Define_attr of define_attr
  | Behav_attr of behav_attr
  | Callback_attr of callback_attr
  | Record_attr of record_attr
  | User_attr of user_attr
  | Vsn_attr of vsn_attr
  | Flow_macro_attr of flow_macro_attr
  | Flow_attr of flow_attr
  | Fun_decl of fun_decl
  | Catch of token * t
  | Block of t enclosed
  | If of if_
  | Case of case
  | Recv of recv
  | Try of try_
  | Anon_fun of anon_fun
  | Module_fun of module_fun
  | Unexp of (op * t)
  | Binexp of binexp
  | Call of call
  | Seq of exp_list
  | Paren of t enclosed
  | Uscore of text
  | Var of text
  | Atom of atom
  | Char of text
  | String of text list
  | Int of text
  | Float of text
  | List of erl_list
  | Binary of exp_list enclosed
  | Binary_elt of binary_elt
  | Tuple of exp_list enclosed
  | Field of field
  | Update of update
  | List_compr of compr (* list comprehension *)
  | List_compr_gen of list_compr_gen
  | Binary_compr of compr (* binary comprehension *)
  | Binary_compr_gen of binary_compr_gen
  | Map of map
  | Macro of macro

and module_ = {
  module_decls : t list;
  module_eof : token;
}

and author_attr = {
  auth_attr_tag : text;
  auth_attr_open : token;
  auth_attr_name : text;
  auth_attr_close : token;
  auth_attr_dot : token;
}

and user_attr = {
  user_attr_tag : text;
  user_attr_open : token;
  user_attr_values : exp_list option;
  user_attr_close : token;
  user_attr_dot : token;
}

and modname_attr = {
  modname_attr_tag : text;
  modname_attr_open : token;
  modname_attr_name: text;
  modname_attr_close : token;
  modname_attr_dot : token;
}

and compile_attr = {
  compile_attr_tag : text;
  compile_attr_open : token;
  compile_attr_name_open : token;
  compile_attr_names : atom node_list;
  compile_attr_name_close : token;
  compile_attr_close : token;
  compile_attr_dot : token;
}

and export_attr = {
  export_attr_tag : text;
  export_attr_open : token;
  export_attr_fun_open : token;
  export_attr_funs : fun_sig node_list;
  export_attr_fun_close : token;
  export_attr_close : token;
  export_attr_dot : token;
}

and import_attr = {
  import_attr_tag : text;
  import_attr_open : token;
  import_attr_module : text;
  import_attr_comma : token;
  import_attr_fun_open : token;
  import_attr_funs : fun_sig node_list;
  import_attr_fun_close : token;
  import_attr_close : token;
  import_attr_dot : token;
}

and fun_sig = {
  fun_sig_name : text;
  fun_sig_sep : token;
  fun_sig_arity : text;
}

and include_attr = {
  include_attr_tag : text;
  include_attr_open : token;
  include_attr_file: text;
  include_attr_close : token;
  include_attr_dot : token;
}

and inclib_attr = {
  inclib_attr_tag : text;
  inclib_attr_open : token;
  inclib_attr_file: text;
  inclib_attr_close : token;
  inclib_attr_dot : token;
}

and onload_attr = {
  onload_attr_tag : text;
  onload_attr_open : token;
  onload_attr_fun : fun_sig;
  onload_attr_close : token;
  onload_attr_dot : token;
}

and opt_cbs_attr = {
  opt_attr_tag : text;
  opt_attr_open : token;
  opt_attr_fun_open : token;
  opt_attr_funs : fun_sig node_list;
  opt_attr_fun_close : token;
  opt_attr_close : token;
  opt_attr_dot : token;
}

and spec_attr = {
  spec_attr_tag : text;
  spec_attr_mname : (text * token) option;
  spec_attr_fname : text;
  spec_attr_clauses : spec_clause node_list;
  spec_attr_dot : token;
}

and spec_clause = {
  spec_clause_open : token;
  spec_clause_args : type_ node_list option;
  spec_clause_close : token;
  spec_clause_arrow : token;
  spec_clause_return : type_;
  spec_clause_guard : (token * guard) option;
}

and type_attr = {
  type_attr_tag : text;
  type_attr_name : text;
  type_attr_open : token;
  type_attr_args : type_ node_list option;
  type_attr_close : token;
  type_attr_colon : token;
  type_attr_type : type_;
  type_attr_dot : token;
}

and define_attr = {
  def_attr_tag : text;
  def_attr_open : token;
  def_attr_name : define_name;
  def_attr_comma : token;
  def_attr_value : t;
  def_attr_close : token;
  def_attr_dot : token;
}

and define_name = {
  def_name : text;
  def_args : text_list enclosed option;
}

and behav_attr = {
  behav_attr_tag : text;
  behav_attr_open : token;
  behav_attr_name : text;
  behav_attr_close : token;
  behav_attr_dot : token;
}

and callback_attr = {
  cb_attr_tag : text;
  cb_attr_name : text;
  cb_attr_clauses : spec_clause node_list;
  cb_attr_dot : token;
}

and record_attr = {
  rec_attr_tag : text;
  rec_attr_open : token;
  rec_attr_name : text;
  rec_attr_comma : token;
  rec_attr_rec_open : token;
  rec_attr_fields : type_field node_list option;
  rec_attr_rec_close : token;
  rec_attr_close : token;
  rec_attr_dot : token;
}

and flow_macro_attr = {
  flow_macro_attr_tag_type : [`Undef | `Ifdef | `Ifndef];
  flow_macro_attr_tag : text;
  flow_macro_attr_open : token;
  flow_macro_attr_macro : text;
  flow_macro_attr_close : token;
  flow_macro_attr_dot : token;
}

and vsn_attr = {
  vsn_attr_tag : text;
  vsn_attr_open : token;
  vsn_attr_value : t;
  vsn_attr_close : token;
  vsn_attr_dot : token;
}

and flow_attr = {
  flow_attr_tag_type : [`Else | `Endif];
  flow_attr_tag : text;
  flow_attr_dot : token;
}

and fun_decl = {
  fun_decl_body : fun_body;
  fun_decl_dot : token;
}

and fun_body = fun_clause node_list

and fun_clause = {
  fun_clause_name : text option;
  fun_clause_open : token;
  fun_clause_ptns : exp_list;
  fun_clause_close : token;
  fun_clause_when : token option;
  fun_clause_guard : guard option;
  fun_clause_arrow : token;
  fun_clause_body : t;
}

and if_ = {
  if_begin : token;
  if_clauses : if_clause node_list;
  if_end : token;
}

and if_clause = {
  if_clause_guard : guard;
  if_clause_arrow : token;
  if_clause_body : t;
}

and case = {
  case_begin : token;
  case_exp : t;
  case_of : token;
  case_clauses : cr_clause node_list;
  case_end : token;
}

and cr_clause = {
  cr_clause_ptn : t;
  cr_clause_when : token option;
  cr_clause_guard : guard option;
  cr_clause_arrow : token;
  cr_clause_body : t;
}

and recv = {
  recv_begin : token;
  recv_clauses : cr_clause node_list;
  recv_after : recv_after option;
  recv_end : token;
}

and recv_after = {
  recv_after_begin : token;
  recv_after_timer : t;
  recv_after_arrow : token;
  recv_after_body : t;
}

and try_ = {
  try_begin : token;
  try_exps : t; 
  try_of : token option;
  try_clauses : cr_clause node_list option;
  try_catch : try_catch;
}

and try_catch = {
  try_catch_begin : token option;
  try_catch_clauses : try_clause node_list option;
  try_catch_after : try_catch_after option;
  try_catch_end : token;
}

and try_catch_after = {
  try_catch_after_begin : token;
  try_catch_after_exps : t;
}

and try_clause = {
  try_clause_cls : t;
  try_clause_exn : (token * t) option;
  try_clause_stack : (token * t) option;
  try_clause_guard : (token * guard) option;
  try_clause_arrow : token;
  try_clause_body : t;
}

and anon_fun = {
  anon_fun_begin : token;
  anon_fun_body : fun_body;
  anon_fun_end : token;
}

and module_fun = {
  module_fun_prefix : token;
  module_fun_mname : t option;
  module_fun_colon : token option;
  module_fun_fname : t;
  module_fun_slash : token;
  module_fun_arity : t;
}

and guard = exp_list node_list

and fun_name = {
  fun_name_mname : t option;
  fun_name_colon : token option;
  fun_name_fname : t;
}

and call = {
  call_fname : fun_name;
  call_open : token;
  call_args : exp_list;
  call_close : token;
}

and binexp = {
  binexp_op : op;
  binexp_left : t;
  binexp_right : t;
}

and erl_list = {
  list_open : token;
  list_head : exp_list;
  list_bar : token option;
  list_tail : t option;
  list_close : token;
}

and compr = {
  compr_open : token;
  compr_exp : t;
  compr_sep : token;
  compr_quals : exp_list;
  compr_close : token;
}

and list_compr_gen = {
  gen_ptn : t;
  gen_arrow : token;
  gen_exp : t;
}

and binary_compr_gen = {
  bin_gen_ptn : t;
  bin_gen_arrow : token;
  bin_gen_exp : t;
}

and update = {
  update_exp : t option;
  update_sharp : token;
  update_name : text;
  update_open : token;
  update_assocs : assoc node_list;
  update_close : token;
}

and assoc = {
  assoc_key : text;
  assoc_val : t;
  assoc_sep : token;
}

and field = {
  field_exp : t option;
  field_sharp : token;
  field_rname : text;
  field_sep : token;
  field_fname : text;
}

and map = {
  map_exp : t option;
  map_nsign : token;
  map_open : token;
  map_pairs : map_pair node_list option;
  map_close : token;
}

and map_pair = {
  map_pair_key : t;
  map_pair_op : [`New of token | `Update of token];
  map_pair_value : t;
}

and binary_elt = {
  bin_elt_val : t;
  bin_elt_colon : token option;
  bin_elt_size : t option;
  bin_elt_slash : token option;
  bin_elt_type : text option;
}

and macro = {
  macro_q : token;
  macro_name : text;
}

and exp_list = t node_list

and type_ =
  | Ty_paren of type_ enclosed
  | Ty_atom of atom
  | Ty_int of text
  | Ty_range of type_range
  | Ty_nil of (token * token)
  | Ty_named of type_named
  | Ty_bits of type_bits
  | Ty_list of type_ enclosed
  | Ty_tuple of type_ node_list option enclosed
  | Ty_fun of type_fun
  | Ty_map of type_map
  | Ty_record of type_record
  | Ty_union of type_union
  | Ty_constr of type_constr
  | Ty_macro of token * text

and type_range = {
  ty_range_start : text;
  ty_range_dot : token;
  ty_range_end : text;
}

and type_bits = {
  ty_bits_open : token;
  ty_bits_start_uscore : text option;
  ty_bits_start_colon : token option;
  ty_bits_start_bits : text option;
  ty_bits_comma : token option;
  ty_bits_cont_uscore1 : text option;
  ty_bits_cont_colon : token option;
  ty_bits_cont_uscore2 : text option;
  ty_bits_cont_mul : token option;
  ty_bits_cont_bits : text option;
  ty_bits_close : token;
}

and type_named = {
  ty_named_module : text option;
  ty_named_colon : token option;
  ty_named_name : text;
  ty_named_open : token;
  ty_named_args : type_ node_list option;
  ty_named_close : token;
}

and type_fun = {
  ty_fun_tag : token;
  ty_fun_open : token;
  ty_fun_body : type_fun_body option;
  ty_fun_close : token;
}

and type_fun_body = {
  ty_fun_body_name : text option;
  ty_fun_body_open : token;
  ty_fun_body_args : [`None | `Dot of token | `Types of type_ node_list];
  ty_fun_body_close : token;
  ty_fun_body_arrow : token;
  ty_fun_body_type : type_;
}

and type_map = {
  ty_map_nsign : token;
  ty_map_open : token;
  ty_map_pairs : type_pair node_list option;
  ty_map_close : token;
}

and type_pair = {
  ty_pair_left : type_;
  ty_pair_op : [`Mandatory of token | `Optional of token];
  ty_pair_right : type_;
}

and type_record = {
  ty_rec_nsign : token;
  ty_rec_name : text;
  ty_rec_open : token;
  ty_rec_fields : type_field node_list option;
  ty_rec_close : token;
}

and type_field = {
  ty_field_name : text;
  ty_field_eq : token option;
  ty_field_init : t option;
  ty_field_colon : token option;
  ty_field_type : type_ option;
}

and type_union = {
  ty_union_left : type_;
  ty_union_op : token;
  ty_union_right : type_;
}

and type_constr = {
  ty_constr_name : text;
  ty_constr_colon : token;
  ty_constr_type : type_;
}
