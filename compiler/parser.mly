%{

open Located

let binexp left op right =
  Ast_t.Binexp {
      binexp_left = left;
      binexp_op = op;
      binexp_right = right }

let paren open_ value close =
  Ast_t.Paren (Ast.enclose open_ value close)

%}

%token <Ast_t.text> UIDENT
%token <Ast_t.text> LIDENT
%token <Ast_t.text> USCORE           (* "_" *)
%token <Ast_t.text> CHAR
%token <Ast_t.text> ATOM
%token <Ast_t.text> STRING
%token <Ast_t.text> INT
%token <Ast_t.text> FLOAT
%token <Ast_t.text> BIT_TYPE
%token <Ast_t.text> AUTHOR_ATTR      (* "-author" *)
%token <Ast_t.text> BEHAV_ATTR       (* "-behaviour" *)
%token <Ast_t.text> CALLBACK_ATTR    (* "-callback" *)
%token <Ast_t.text> COMPILE_ATTR     (* "-compile" *)
%token <Ast_t.text> DEFINE_ATTR      (* "-define" *)
%token <Ast_t.text> DEPR_ATTR        (* "-deprecated" *)
%token <Ast_t.text> EXPORT_ATTR      (* "-export" *)
%token <Ast_t.text> EXPORT_TYPE_ATTR (* "-export_type" *)
%token <Ast_t.text> FILE_ATTR        (* "-file" *)
%token <Ast_t.text> IMPORT_ATTR      (* "-import" *)
%token <Ast_t.text> INCLUDE_ATTR     (* "-include" *)
%token <Ast_t.text> INCLIB_ATTR      (* "-include_lib" *)
%token <Ast_t.text> MODULE_ATTR      (* "-module" *)
%token <Ast_t.text> ONLOAD_ATTR      (* "-on_load" *)
%token <Ast_t.text> OPAQUE_ATTR      (* "-opaque" *)
%token <Ast_t.text> OPT_CBS_ATTR     (* "-optional_callbacks" *)
%token <Ast_t.text> SPEC_ATTR        (* "-spec" *)
%token <Ast_t.text> TYPE_ATTR        (* "-type" *)
%token <Ast_t.text> RECORD_ATTR      (* "-record" *)
%token <Ast_t.text> UNDEF_ATTR       (* "-undef" *)
%token <Ast_t.text> IFDEF_ATTR       (* "-ifdef" *)
%token <Ast_t.text> IFNDEF_ATTR      (* "-ifndef" *)
%token <Ast_t.text> ELSE_ATTR        (* "-else" *)
%token <Ast_t.text> ENDIF_ATTR       (* "-endif" *)
%token <Ast_t.text> VSN_ATTR         (* "-vsn" *)
%token <Ast_t.text> USER_ATTR        (* other module attributes *)
%token <Ast_t.token> LPAREN
%token <Ast_t.token> RPAREN
%token <Ast_t.token> LBRACK
%token <Ast_t.token> RBRACK
%token <Ast_t.token> LBRACE
%token <Ast_t.token> RBRACE
%token <Ast_t.token> COMMA
%token <Ast_t.token> DOT              (* "." *)
%token <Ast_t.token> DOT2             (* ".." *)
%token <Ast_t.token> DOT3             (* "..." *)
%token <Ast_t.token> COLON
%token <Ast_t.token> COLON2
%token <Ast_t.token> SEMI
%token <Ast_t.token> NSIGN            (* "#" *)
%token <Ast_t.token> AND              (* "and" *)
%token <Ast_t.token> ANDALSO          (* "andalso" *)
%token <Ast_t.token> OR               (* "or" *)
%token <Ast_t.token> ORELSE           (* "orelse" *)
%token <Ast_t.token> NOT              (* "not" *)
%token <Ast_t.token> LAND             (* "band" *)
%token <Ast_t.token> LOR              (* "bor" *)
%token <Ast_t.token> LXOR             (* "bxor" *)
%token <Ast_t.token> LNOT             (* "bnot" *)
%token <Ast_t.token> LSHIFT           (* "bsl" *)
%token <Ast_t.token> RSHIFT           (* "bsr" *)
%token <Ast_t.token> EQ               (* "=" *)
%token <Ast_t.token> BANG             (* "!" *)
%token <Ast_t.token> Q                (* "?" *)
%token <Ast_t.token> BAR              (* "|" *)
%token <Ast_t.token> DBAR             (* "||" *)
%token <Ast_t.token> EQQ              (* "==" *)
%token <Ast_t.token> CEQ              (* ":=" *)
%token <Ast_t.token> NE               (* "/=" *)
%token <Ast_t.token> XEQ              (* "=:=" *)
%token <Ast_t.token> XNE              (* "=/=" *)
%token <Ast_t.token> LT               (* "<" *)
%token <Ast_t.token> LE               (* "=<" *)
%token <Ast_t.token> GT               (* ">" *)
%token <Ast_t.token> GE               (* ">=" *)
%token <Ast_t.token> PLUS             (* "+" *)
%token <Ast_t.token> PLUS2            (* "++" *)
%token <Ast_t.token> MINUS            (* "-" *)
%token <Ast_t.token> MINUS2           (* "--" *)
%token <Ast_t.token> MUL              (* "*" *)
%token <Ast_t.token> DIV              (* "/" *)
%token <Ast_t.token> QUO              (* "div" *)
%token <Ast_t.token> REM              (* "rem" *)
%token <Ast_t.token> RARROW           (* "->" *)
%token <Ast_t.token> RARROW2          (* "=>" *)
%token <Ast_t.token> LARROW           (* "<-" *)
%token <Ast_t.token> LARROW2          (* "<=" *)
%token <Ast_t.token> DLT              (* ">>" *)
%token <Ast_t.token> DGT              (* "<<" *)
%token <Ast_t.token> AFTER            (* "after" *)
%token <Ast_t.token> BEGIN            (* "begin" *)
%token <Ast_t.token> CASE             (* "case" *)
%token <Ast_t.token> CATCH            (* "catch" *)
%token <Ast_t.token> COND             (* "cond" *)
%token <Ast_t.token> END              (* "end" *)
%token <Ast_t.token> FUN              (* "fun" *)
%token <Ast_t.token> IF               (* "if" *)
%token <Ast_t.token> LET              (* "let" *)
%token <Ast_t.token> OF               (* "of" *)
%token <Ast_t.token> RECEIVE          (* "receive" *)
%token <Ast_t.token> TRY              (* "try" *)
%token <Ast_t.token> WHEN             (* "when" *)
%token <Ast_t.token> EOF

%nonassoc CATCH
%right EQ BANG
%left ORELSE
%left ANDALSO
%right PLUS2 MINUS2
%left PLUS MINUS
%nonassoc SEMI
%nonassoc NSIGN
%nonassoc COLON

%right BAR
%nonassoc COLON2

%start <Ast_t.t> prog

%%

prog:
  | module_ { $1 }

module_:
  | EOF
  { Ast_t.Module { module_decls = []; module_eof = $1 } }
  | module_decl+ EOF
  { Ast_t.Module { module_decls = $1; module_eof = $2 } }

module_decl:
  | module_attr { $1 }
  | fun_decl { $1 }

module_attr:
  | modname_attr { $1 }
  | author_attr { $1 }
  | compile_attr { $1 }
  | export_attr { $1 }
  | export_type_attr { $1 }
  | file_attr { $1 }
  | import_attr { $1 }
  | include_attr { $1 }
  | include_lib_attr { $1 }
  | define_attr { $1 }
  | depr_attr { $1 }
  | spec_attr { $1 }
  | type_attr { $1 }
  | onload_attr { $1 }
  | opaque_attr { $1 }
  | opt_cbs_attr { $1 }
  | behav_attr { $1 }
  | record_attr { $1 }
  | callback_attr { $1 }
  | flow_macro_attr { $1 }
  | vsn_attr { $1 }
  | user_attr { $1 }

modname_attr:
  | MODULE_ATTR LPAREN LIDENT RPAREN DOT
  { Ast_t.Modname_attr {
      modname_attr_tag = $1;
      modname_attr_open = $2;
      modname_attr_name = $3;
      modname_attr_close = $4;
      modname_attr_dot = $5;
    }
  }

author_attr:
  | AUTHOR_ATTR LPAREN STRING RPAREN DOT
  { Ast_t.Author_attr {
      auth_attr_tag = $1;
      auth_attr_open = $2;
      auth_attr_name = $3;
      auth_attr_close = $4;
      auth_attr_dot = $5;
    }
  }

compile_attr:
  | COMPILE_ATTR LPAREN exp RPAREN DOT
  { Ast_t.Compile_attr {
      compile_attr_tag = $1;
      compile_attr_open = $2;
      compile_attr_value = $3;
      compile_attr_close = $4;
      compile_attr_dot = $5;
    }
  }

raw_atoms:
  | rev_raw_atoms { Seplist.rev $1 }

rev_raw_atoms:
  | raw_atom { Seplist.one $1 }
  | rev_raw_atoms COMMA raw_atom { Seplist.cons $3 ~sep:$2 $1 }

export_attr:
  | EXPORT_ATTR LPAREN LBRACK fun_sigs RBRACK RPAREN DOT
  { Ast_t.Export_attr {
      export_attr_tag = $1;
      export_attr_open = $2;
      export_attr_fun_open = $3;
      export_attr_funs = $4;
      export_attr_fun_close = $5;
      export_attr_close = $6;
      export_attr_dot = $7;
    }
  }

export_type_attr:
  | EXPORT_TYPE_ATTR LPAREN LBRACK fun_sigs RBRACK RPAREN DOT
  { Ast_t.Export_type_attr {
      export_attr_tag = $1;
      export_attr_open = $2;
      export_attr_fun_open = $3;
      export_attr_funs = $4;
      export_attr_fun_close = $5;
      export_attr_close = $6;
      export_attr_dot = $7;
    }
  }

fun_sigs:
  | rev_fun_sigs { Seplist.rev $1 }

rev_fun_sigs:
  | fun_sig { Seplist.one $1 }
  | rev_fun_sigs COMMA fun_sig { Seplist.cons $3 ~sep:$2 $1 }

fun_sig:
  | LIDENT DIV INT
  { {
      Ast_t.fun_sig_name = $1;
      fun_sig_sep = $2;
      fun_sig_arity = $3;
    }
  }

file_attr:
  | FILE_ATTR LPAREN STRING COMMA INT RPAREN DOT
  { Ast_t.File_attr {
      file_attr_tag = $1;
      file_attr_open = $2;
      file_attr_path = $3;
      file_attr_comma = $4;
      file_attr_line = $5;
      file_attr_close = $6;
      file_attr_dot = $7;
    }
  }

import_attr:
  | IMPORT_ATTR LPAREN LIDENT COMMA LBRACK fun_sigs RBRACK RPAREN DOT
  { Ast_t.Import_attr {
      import_attr_tag = $1;
      import_attr_open = $2;
      import_attr_module = $3;
      import_attr_comma = $4;
      import_attr_fun_open = $5;
      import_attr_funs = $6;
      import_attr_fun_close = $7;
      import_attr_close = $8;
      import_attr_dot = $9;
    }
  }

include_attr:
  | INCLUDE_ATTR LPAREN STRING RPAREN DOT
  { Ast_t.Include_attr {
      include_attr_tag = $1;
      include_attr_open = $2;
      include_attr_file = $3;
      include_attr_close = $4;
      include_attr_dot = $5;
    }
  }

include_lib_attr:
  | INCLIB_ATTR LPAREN STRING RPAREN DOT
  { Ast_t.Inclib_attr {
      inclib_attr_tag = $1;
      inclib_attr_open = $2;
      inclib_attr_file = $3;
      inclib_attr_close = $4;
      inclib_attr_dot = $5;
    }
  }

define_attr:
  | DEFINE_ATTR LPAREN define_name COMMA exp RPAREN DOT
  { Ast_t.Define_attr {
      def_attr_tag = $1;
      def_attr_open = $2;
      def_attr_name = $3;
      def_attr_comma = $4;
      def_attr_value = $5;
      def_attr_close = $6;
      def_attr_dot = $7;
    }
  }

define_name:
  | macro_name { { Ast_t.def_name = $1; def_args = None } }
  | macro_name LPAREN define_args RPAREN
  { { Ast_t.def_name = $1;
      def_args = Some (Ast.enclose $2 $3 $4);
    }
  }

define_args:
  | rev_define_args { Seplist.rev $1 }

rev_define_args:
  | define_arg { Seplist.one $1 }
  | rev_define_args SEMI define_arg { Seplist.cons $3 ~sep:$2 $1 }

define_arg:
  | UIDENT { $1 }
  | ATOM { $1 }

depr_attr:
  | DEPR_ATTR LPAREN list_skel RPAREN DOT
  { Ast_t.Depr_attr {
      depr_attr_tag = $1;
      depr_attr_open = $2;
      depr_attr_list = $3;
      depr_attr_close = $4;
      depr_attr_dot = $5;
    }
  }

spec_attr:
  | SPEC_ATTR LIDENT spec_clauses DOT
  { Ast_t.(Spec_attr {
      spec_attr_tag = $1;
      spec_attr_mname = None;
      spec_attr_fname = $2;
      spec_attr_clauses = $3;
      spec_attr_dot = $4; })
  }
  | SPEC_ATTR LIDENT COLON LIDENT spec_clauses DOT
  { Ast_t.(Spec_attr {
      spec_attr_tag = $1;
      spec_attr_mname = Some ($2, $3);
      spec_attr_fname = $4;
      spec_attr_clauses = $5;
      spec_attr_dot = $6; })
  }

spec_clauses:
  | rev_spec_clauses { Seplist.rev $1 }

rev_spec_clauses:
  | spec_clause { Seplist.one $1 }
  | rev_spec_clauses SEMI spec_clause { Seplist.cons $3 ~sep:$2 $1 }

spec_clause:
  | LPAREN RPAREN RARROW spec_type
  { Ast_t.({
      spec_clause_open = $1;
      spec_clause_args = None;
      spec_clause_close = $2;
      spec_clause_arrow = $3;
      spec_clause_return = $4;
      spec_clause_guard = None; })
  }
  | LPAREN RPAREN RARROW spec_type WHEN spec_guard
  { Ast_t.({
      spec_clause_open = $1;
      spec_clause_args = None;
      spec_clause_close = $2;
      spec_clause_arrow = $3;
      spec_clause_return = $4;
      spec_clause_guard = Some ($5, $6); })
  }
  | LPAREN spec_args RPAREN RARROW spec_type
  { Ast_t.({
      spec_clause_open = $1;
      spec_clause_args = Some $2;
      spec_clause_close = $3;
      spec_clause_arrow = $4;
      spec_clause_return = $5;
      spec_clause_guard = None; })
  }
  | LPAREN spec_args RPAREN RARROW spec_type WHEN spec_guard
  { Ast_t.({
      spec_clause_open = $1;
      spec_clause_args = Some $2;
      spec_clause_close = $3;
      spec_clause_arrow = $4;
      spec_clause_return = $5;
      spec_clause_guard = Some ($6, $7); })
  }

spec_args:
  | rev_spec_args { Seplist.rev $1 }

rev_spec_args:
  | spec_arg { Seplist.one $1 }
  | rev_spec_args COMMA spec_arg { Seplist.cons $3 ~sep:$2 $1 }

spec_arg:
  | spec_type { $1 }

spec_guard:
  | rev_spec_guard { Seplist.rev $1 }

rev_spec_guard:
  | spec_guard_type { Seplist.one $1 }
  | rev_spec_guard COMMA spec_guard_type
  { Seplist.cons $3 ~sep:$2 $1 }

spec_guard_type:
  | UIDENT COLON2 spec_type
  { {
      Ast_t.ty_constr_name = $1;
      ty_constr_type = Some ($2, $3);
    }
  }

spec_type:
  | raw_atom { Ast_t.Ty_atom $1 }
  | spec_type_constraint { $1 }
  | spec_type_named { $1 }
  | spec_type_list { $1 }
  | spec_type_tuple { $1 }
  | spec_type_fun { $1 }
  | spec_type_bitstr { $1 }
  | spec_type_map { $1 }
  | spec_type_record { $1 }
  | spec_type_union { $1 }
  | LPAREN spec_type RPAREN
  { Ast_t.(Ty_paren (Ast.enclose $1 $2 $3)) }
  | INT { Ast_t.Ty_int $1 }
  | INT DOT2 INT
  { Ast_t.Ty_range {
      ty_range_start = $1;
      ty_range_dot = $2;
      ty_range_end = $3;
    }
  }
  | LBRACK RBRACK
  { Ast_t.Ty_nil ($1, $2) }
  | Q macro_name
  { Ast_t.Ty_macro ($1, $2) }

spec_type_constraint:
  | UIDENT
  { Ast_t.Ty_constr {
      ty_constr_name = $1;
      ty_constr_type = None;
    }
  }
  | UIDENT COLON2 spec_type
  { Ast_t.Ty_constr {
      ty_constr_name = $1;
      ty_constr_type = Some ($2, $3);
    }
  }

spec_type_named:
  | LIDENT LPAREN RPAREN
  { Ast_t.Ty_named {
      ty_named_module = None;
      ty_named_colon = None;
      ty_named_name = $1;
      ty_named_open = $2;
      ty_named_args = None;
      ty_named_close = $3;
    }
  }
  | LIDENT LPAREN spec_type_args RPAREN
  { Ast_t.Ty_named {
      ty_named_module = None;
      ty_named_colon = None;
      ty_named_name = $1;
      ty_named_open = $2;
      ty_named_args = Some $3;
      ty_named_close = $4;
    }
  }
  | LIDENT COLON LIDENT LPAREN RPAREN
  { Ast_t.Ty_named {
      ty_named_module = Some $1;
      ty_named_colon = Some $2;
      ty_named_name = $3;
      ty_named_open = $4;
      ty_named_args = None;
      ty_named_close = $5;
    }
  }
  | LIDENT COLON LIDENT LPAREN spec_type_args RPAREN
  { Ast_t.Ty_named {
      ty_named_module = Some $1;
      ty_named_colon = Some $2;
      ty_named_name = $3;
      ty_named_open = $4;
      ty_named_args = Some $5;
      ty_named_close = $6;
    }
  }

spec_type_args:
  | rev_spec_type_args { Seplist.rev $1 }

rev_spec_type_args:
  | spec_type { Seplist.one $1 }
  | rev_spec_type_args COMMA spec_type { Seplist.cons $3 ~sep:$2 $1 }

spec_type_list:
  | LBRACK spec_type RBRACK
  { Ast_t.(Ty_list (Ast.enclose $1 $2 $3)) }

  spec_type_tuple:
  | LBRACE RBRACE
  { Ast_t.(Ty_tuple (Ast.enclose $1 None $2)) }
  | LBRACE spec_type_args RBRACE
  { Ast_t.(Ty_tuple (Ast.enclose $1 (Some $2) $3)) }

spec_type_fun:
  | FUN LPAREN RPAREN
  { Ast_t.Ty_fun {
      ty_fun_tag = $1;
      ty_fun_open = $2;
      ty_fun_body = None;
      ty_fun_close = $3;
    }
  }
  | FUN LPAREN spec_fun_body RPAREN
  { Ast_t.Ty_fun {
      ty_fun_tag = $1;
      ty_fun_open = $2;
      ty_fun_body = Some $3;
      ty_fun_close = $4;
    }
  }

spec_type_bitstr:
  | DLT DGT
  { Ast_t.Ty_bits {
      ty_bits_open = $1;
      ty_bits_start_uscore = None;
      ty_bits_start_colon = None;
      ty_bits_start_bits = None;
      ty_bits_comma = None;
      ty_bits_cont_uscore1 = None;
      ty_bits_cont_colon = None;
      ty_bits_cont_uscore2 = None;
      ty_bits_cont_mul = None;
      ty_bits_cont_bits = None;
      ty_bits_close = $2;
    }
  }
  | DLT USCORE COLON INT DGT
  { Ast_t.Ty_bits {
      ty_bits_open = $1;
      ty_bits_start_uscore = Some $2;
      ty_bits_start_colon = Some $3;
      ty_bits_start_bits = Some $4;
      ty_bits_comma = None;
      ty_bits_cont_uscore1 = None;
      ty_bits_cont_colon = None;
      ty_bits_cont_uscore2 = None;
      ty_bits_cont_mul = None;
      ty_bits_cont_bits = None;
      ty_bits_close = $5;
    }
  }
  | DLT USCORE COLON USCORE MUL INT DGT
  { Ast_t.Ty_bits {
      ty_bits_open = $1;
      ty_bits_start_uscore = None;
      ty_bits_start_colon = None;
      ty_bits_start_bits = None;
      ty_bits_comma = None;
      ty_bits_cont_uscore1 = Some $2;
      ty_bits_cont_colon = Some $3;
      ty_bits_cont_uscore2 = Some $4;
      ty_bits_cont_mul = Some $5;
      ty_bits_cont_bits = Some $6;
      ty_bits_close = $5;
    }
  }
  | DLT USCORE COLON INT COMMA USCORE COLON USCORE MUL INT DGT
  { Ast_t.Ty_bits {
      ty_bits_open = $1;
      ty_bits_start_uscore = Some $2;
      ty_bits_start_colon = Some $3;
      ty_bits_start_bits = Some $4;
      ty_bits_comma = Some $5;
      ty_bits_cont_uscore1 = Some $6;
      ty_bits_cont_colon = Some $7;
      ty_bits_cont_uscore2 = Some $8;
      ty_bits_cont_mul = Some $9;
      ty_bits_cont_bits = Some $10;
      ty_bits_close = $5;
    }
  }

spec_fun_body:
  | LPAREN RPAREN RARROW spec_type
  { { Ast_t.ty_fun_body_name = None;
      ty_fun_body_open = $1;
      ty_fun_body_args = `None;
      ty_fun_body_close = $2;
      ty_fun_body_arrow = $3;
      ty_fun_body_type = $4;
    }
  }
  | UIDENT LPAREN RPAREN RARROW spec_type
  { { Ast_t.ty_fun_body_name = Some $1;
      ty_fun_body_open = $2;
      ty_fun_body_args = `None;
      ty_fun_body_close = $3;
      ty_fun_body_arrow = $4;
      ty_fun_body_type = $5;
    }
  }
  | LPAREN DOT3 RPAREN RARROW spec_type
  { { Ast_t.ty_fun_body_name = None;
      ty_fun_body_open = $1;
      ty_fun_body_args = `Dot $2;
      ty_fun_body_close = $3;
      ty_fun_body_arrow = $4;
      ty_fun_body_type = $5;
    }
  }
  | UIDENT LPAREN DOT3 RPAREN RARROW spec_type
  { { Ast_t.ty_fun_body_name = Some $1;
      ty_fun_body_open = $2;
      ty_fun_body_args = `Dot $3;
      ty_fun_body_close = $4;
      ty_fun_body_arrow = $5;
      ty_fun_body_type = $6;
    }
  }
  | LPAREN spec_args RPAREN RARROW spec_type
  { { Ast_t.ty_fun_body_name = None;
      ty_fun_body_open = $1;
      ty_fun_body_args = `Types $2;
      ty_fun_body_close = $3;
      ty_fun_body_arrow = $4;
      ty_fun_body_type = $5;
    }
  }
  | UIDENT LPAREN spec_args RPAREN RARROW spec_type
  { { Ast_t.ty_fun_body_name = Some $1;
      ty_fun_body_open = $2;
      ty_fun_body_args = `Types $3;
      ty_fun_body_close = $4;
      ty_fun_body_arrow = $5;
      ty_fun_body_type = $6;
    }
  }

spec_type_map:
  | NSIGN LBRACE RBRACE
  { Ast_t.Ty_map {
      ty_map_nsign = $1;
      ty_map_open = $2;
      ty_map_pairs = None;
      ty_map_close = $3;
    }
  }
  | NSIGN LBRACE spec_pairs RBRACE
  { Ast_t.Ty_map {
      ty_map_nsign = $1;
      ty_map_open = $2;
      ty_map_pairs = Some $3;
      ty_map_close = $4;
    }
  }

spec_pairs:
  | rev_spec_pairs { Seplist.rev $1 }

rev_spec_pairs:
  | spec_pair { Seplist.one $1 }
  | rev_spec_pairs COMMA spec_pair { Seplist.cons $3 ~sep:$2 $1 }

spec_pair:
  | spec_type CEQ spec_type
  { { Ast_t.ty_pair_left = $1;
      ty_pair_op = `Mandatory $2;
      ty_pair_right = $3;
    }
  }
  | spec_type RARROW2 spec_type
  { { Ast_t.ty_pair_left = $1;
      ty_pair_op = `Optional $2;
      ty_pair_right = $3;
    }
  }

spec_type_record:
  | NSIGN LIDENT LBRACE RBRACE
  { Ast_t.Ty_record {
      ty_rec_nsign = $1;
      ty_rec_name = $2;
      ty_rec_open = $3;
      ty_rec_fields = None;
      ty_rec_close = $4;
    }
  }
  | NSIGN LIDENT LBRACE type_fields RBRACE
  { Ast_t.Ty_record {
      ty_rec_nsign = $1;
      ty_rec_name = $2;
      ty_rec_open = $3;
      ty_rec_fields = Some $4;
      ty_rec_close = $5;
    }
  }

spec_type_union:
  | spec_type BAR spec_type
  { Ast_t.Ty_union {
      ty_union_left = $1;
      ty_union_op = $2;
      ty_union_right = $3;
    }
  }

type_attr:
  | TYPE_ATTR LIDENT LPAREN RPAREN COLON2 spec_type DOT
  { Ast_t.Type_attr {
      type_attr_tag = $1;
      type_attr_name = $2;
      type_attr_open = $3;
      type_attr_args = None;
      type_attr_close = $4;
      type_attr_colon = $5;
      type_attr_type = $6;
      type_attr_dot = $7;
    }
  }
  | TYPE_ATTR LIDENT LPAREN spec_args RPAREN COLON2 spec_type DOT
  { Ast_t.Type_attr {
      type_attr_tag = $1;
      type_attr_name = $2;
      type_attr_open = $3;
      type_attr_args = Some $4;
      type_attr_close = $5;
      type_attr_colon = $6;
      type_attr_type = $7;
      type_attr_dot = $8;
    }
  }

onload_attr:
  | ONLOAD_ATTR LPAREN fun_sig RPAREN DOT
  { Ast_t.Onload_attr {
      onload_attr_tag = $1;
      onload_attr_open = $2;
      onload_attr_fun = $3;
      onload_attr_close = $4;
      onload_attr_dot = $5;
    }
  }

opaque_attr:
  | OPAQUE_ATTR LIDENT LPAREN RPAREN COLON2 spec_type DOT
  { Ast_t.Opaque_attr {
      type_attr_tag = $1;
      type_attr_name = $2;
      type_attr_open = $3;
      type_attr_args = None;
      type_attr_close = $4;
      type_attr_colon = $5;
      type_attr_type = $6;
      type_attr_dot = $7;
    }
  }
  | OPAQUE_ATTR LIDENT LPAREN spec_args RPAREN COLON2 spec_type DOT
  { Ast_t.Opaque_attr {
      type_attr_tag = $1;
      type_attr_name = $2;
      type_attr_open = $3;
      type_attr_args = Some $4;
      type_attr_close = $5;
      type_attr_colon = $6;
      type_attr_type = $7;
      type_attr_dot = $8;
    }
  }

opt_cbs_attr:
  | OPT_CBS_ATTR LPAREN LBRACK fun_sigs RBRACK RPAREN DOT
  { Ast_t.Opt_cbs_attr {
      opt_attr_tag = $1;
      opt_attr_open = $2;
      opt_attr_fun_open = $3;
      opt_attr_funs = $4;
      opt_attr_fun_close = $5;
      opt_attr_close = $6;
      opt_attr_dot = $7;
    }
  }


behav_attr:
  | BEHAV_ATTR LPAREN LIDENT RPAREN DOT
  { Ast_t.Behav_attr {
      behav_attr_tag = $1;
      behav_attr_open = $2;
      behav_attr_name = $3;
      behav_attr_close = $4;
      behav_attr_dot = $5;
    }
  }

callback_attr:
  | CALLBACK_ATTR LIDENT spec_clauses DOT
  { Ast_t.(Callback_attr {
      cb_attr_tag = $1;
      cb_attr_name = $2;
      cb_attr_clauses = $3;
      cb_attr_dot = $4; })
  }

record_attr:
  | RECORD_ATTR LPAREN LIDENT COMMA LBRACE RBRACE RPAREN DOT
  { Ast_t.Record_attr {
      rec_attr_tag = $1;
      rec_attr_open = $2;
      rec_attr_name = $3;
      rec_attr_comma = $4;
      rec_attr_rec_open = $5;
      rec_attr_fields = None;
      rec_attr_rec_close = $6;
      rec_attr_close = $7;
      rec_attr_dot = $8;
    }
  }
  | RECORD_ATTR LPAREN LIDENT COMMA LBRACE type_fields RBRACE RPAREN DOT
  { Ast_t.Record_attr {
      rec_attr_tag = $1;
      rec_attr_open = $2;
      rec_attr_name = $3;
      rec_attr_comma = $4;
      rec_attr_rec_open = $5;
      rec_attr_fields = Some $6;
      rec_attr_rec_close = $7;
      rec_attr_close = $8;
      rec_attr_dot = $9;
    }
  }

type_fields:
  | rev_type_fields { Seplist.rev $1 }

rev_type_fields:
  | type_field { Seplist.one $1 }
  | rev_type_fields COMMA type_field { Seplist.cons $3 ~sep:$2 $1 }

type_field:
  | LIDENT
  { { Ast_t.ty_field_name = $1;
        ty_field_eq = None;
        ty_field_init = None;
        ty_field_colon = None;
        ty_field_type = None;
    }
  }
  | LIDENT COLON2 spec_type
  { { Ast_t.ty_field_name = $1;
        ty_field_eq = None;
        ty_field_init = None;
        ty_field_colon = Some $2;
        ty_field_type = Some $3;
    }
  }
  | LIDENT EQ exp
  { { Ast_t.ty_field_name = $1;
        ty_field_eq = Some $2;
        ty_field_init = Some $3;
        ty_field_colon = None;
        ty_field_type = None;
    }
  }
  | LIDENT EQ exp COLON2 spec_type
  { { Ast_t.ty_field_name = $1;
        ty_field_eq = Some $2;
        ty_field_init = Some $3;
        ty_field_colon = Some $4;
        ty_field_type = Some $5;
    }
  }

flow_macro_attr:
  | UNDEF_ATTR LPAREN macro_name RPAREN DOT
  { Ast_t.Flow_macro_attr {
      flow_macro_attr_tag_type = `Undef;
      flow_macro_attr_tag = $1;
      flow_macro_attr_open = $2;
      flow_macro_attr_macro = $3;
      flow_macro_attr_close = $4;
      flow_macro_attr_dot = $5;
    }
  }
  | IFDEF_ATTR LPAREN macro_name RPAREN DOT
  { Ast_t.Flow_macro_attr {
      flow_macro_attr_tag_type = `Ifdef;
      flow_macro_attr_tag = $1;
      flow_macro_attr_open = $2;
      flow_macro_attr_macro = $3;
      flow_macro_attr_close = $4;
      flow_macro_attr_dot = $5;
    }
  }
  | IFNDEF_ATTR LPAREN macro_name RPAREN DOT
  { Ast_t.Flow_macro_attr {
      flow_macro_attr_tag_type = `Ifndef;
      flow_macro_attr_tag = $1;
      flow_macro_attr_open = $2;
      flow_macro_attr_macro = $3;
      flow_macro_attr_close = $4;
      flow_macro_attr_dot = $5;
    }
  }
  | ELSE_ATTR DOT
  { Ast_t.Flow_attr {
      flow_attr_tag_type = `Else;
      flow_attr_tag = $1;
      flow_attr_dot = $2;
    }
  }
  | ENDIF_ATTR DOT
  { Ast_t.Flow_attr {
      flow_attr_tag_type = `Endif;
      flow_attr_tag = $1;
      flow_attr_dot = $2;
    }
  }

vsn_attr:
  | VSN_ATTR LPAREN exp RPAREN DOT
  { Ast_t.Vsn_attr {
      vsn_attr_tag = $1;
      vsn_attr_open = $2;
      vsn_attr_value = $3;
      vsn_attr_close = $4;
      vsn_attr_dot = $5;
    }
  }

user_attr:
  | USER_ATTR LPAREN RPAREN DOT
  { Ast_t.User_attr {
      user_attr_tag = $1;
      user_attr_open = $2;
      user_attr_values = None;
      user_attr_close = $3;
      user_attr_dot = $4;
    }
  }
  | USER_ATTR LPAREN exps RPAREN DOT
  { Ast_t.User_attr {
      user_attr_tag = $1;
      user_attr_open = $2;
      user_attr_values = Some $3;
      user_attr_close = $4;
      user_attr_dot = $5;
    }
  }

fun_decl:
  | fun_clauses DOT
  { Ast_t.Fun_decl {
      fun_decl_body = $1;
      fun_decl_dot = $2 }
  }

fun_clauses:
  | rev_fun_clauses { Seplist.rev $1 }

rev_fun_clauses:
  | fun_clause { Seplist.one $1 }
  | rev_fun_clauses SEMI fun_clause { Seplist.cons $3 ~sep:$2 $1 }

fun_clause:
  | fun_clause_def { $1 }
  | LIDENT fun_clause_def
  { { $2 with Ast_t.fun_clause_name = Some $1 } }
  | UIDENT fun_clause_def
  { { $2 with Ast_t.fun_clause_name = Some $1 } }

fun_clause_def:
  | LPAREN patterns_opt RPAREN RARROW body
  { Ast_t.({
      fun_clause_name = None;
      fun_clause_open = $1;
      fun_clause_ptns = $2;
      fun_clause_close = $3;
      fun_clause_when = None;
      fun_clause_guard = None;
      fun_clause_arrow = $4;
      fun_clause_body = $5 })
  }
  | LPAREN patterns_opt RPAREN WHEN guard RARROW body
  { Ast_t.({
      fun_clause_name = None;
      fun_clause_open = $1;
      fun_clause_ptns = $2;
      fun_clause_close = $3;
      fun_clause_when = Some $4;
      fun_clause_guard = Some $5;
      fun_clause_arrow = $6;
      fun_clause_body = $7 })
  }

guard:
  | rev_guard { Seplist.rev $1 }

rev_guard:
  | guard_clauses { Seplist.one $1 }
  | rev_guard SEMI guard_clauses { Seplist.cons $3 ~sep:$2 $1 }

guard_clauses:
  | rev_guard_clauses { Seplist.rev $1 }

rev_guard_clauses:
  | exp { Seplist.one $1 }
  | rev_guard_clauses COMMA exp { Seplist.cons $3 ~sep:$2 $1 }

body:
  | seq { $1 }

seq:
  | exps { Ast_t.Seq $1 }

exps:
  | rev_exps { Seplist.rev $1 }

rev_exps:
  | exp { Seplist.one $1 }
  | rev_exps COMMA exp { Seplist.cons $3 ~sep:$2 $1 }

exps_opt:
  | exps { $1 }
  | (* empty *) { Seplist.empty }

exp:
  | CATCH exp { Ast_t.Catch ($1, $2) }
  | match_exp { $1 }

match_exp:
  | match_exp EQ match_exp
  { binexp $1 (create $2 Ast_t.Op_eq) $3 }
  | send_exp { $1 }

send_exp:
  | send_exp BANG send_exp
  { binexp $1 (create $2 Ast_t.Op_ep) $3 }
  | or_cond_exp { $1 }

or_cond_exp:
  | or_cond_exp ORELSE or_cond_exp
  { binexp $1 (create $2 Ast_t.Op_orelse) $3 }
  | and_cond_exp { $1 }

and_cond_exp:
  | and_cond_exp ANDALSO and_cond_exp
  { binexp $1 (create $2 Ast_t.Op_andalso) $3 }
  | compare_exp { $1 }

compare_exp:
  | list_conc_exp compare_op list_conc_exp { binexp $1 $2 $3 }
  | list_conc_exp { $1 }

compare_op:
  | EQQ { create $1 Ast_t.Op_eqq }
  | NE { create $1 Ast_t.Op_ne }
  | XEQ { create $1 Ast_t.Op_xeq }
  | XNE { create $1 Ast_t.Op_xne }
  | GT { create $1 Ast_t.Op_gt }
  | GE { create $1 Ast_t.Op_ge }
  | LT { create $1 Ast_t.Op_lt }
  | LE { create $1 Ast_t.Op_le }

list_conc_exp:
  | shift_exp list_conc_op list_conc_exp { binexp $1 $2 $3 }
  | shift_exp { $1 }

list_conc_op:
  | PLUS2 { create $1 Ast_t.Op_list_concat }
  | MINUS2 { create $1 Ast_t.Op_list_sub }

shift_exp:
  | shift_exp shift_op mul_exp { binexp $1 $2 $3 }
  | mul_exp { $1 }

shift_op:
  | PLUS { create $1 Ast_t.Op_add }
  | MINUS { create $1 Ast_t.Op_sub }
  | LOR { create $1 Ast_t.Op_lor }
  | LXOR { create $1 Ast_t.Op_lxor }
  | LSHIFT { create $1 Ast_t.Op_lshift }
  | RSHIFT { create $1 Ast_t.Op_rshift }

mul_exp:
  | mul_exp mul_op prefix_exp { binexp $1 $2 $3 }
  | prefix_exp { $1 }

mul_op:
  | MUL { create $1 Ast_t.Op_mul }
  | DIV { create $1 Ast_t.Op_div }
  | QUO { create $1 Ast_t.Op_quo }
  | REM { create $1 Ast_t.Op_rem }
  | AND { create $1 Ast_t.Op_and }
  | LAND { create $1 Ast_t.Op_land }

prefix_exp:
  | prefix_op record_exp { Ast_t.Unexp ($1, $2) }
  | record_exp { $1 }

prefix_op:
  | PLUS { create $1 Ast_t.Op_pos }
  | MINUS { create $1 Ast_t.Op_neg }
  | NOT { create $1 Ast_t.Op_not }
  | LNOT { create $1 Ast_t.Op_lnot }

record_exp:
  | record_exp NSIGN LIDENT DOT LIDENT
  { Ast_t.Field {
      field_exp = Some $1;
      field_sharp = $2;
      field_rname = $3;
      field_sep = $4;
      field_fname = $5; }
  }
  | NSIGN LIDENT DOT LIDENT
  { Ast_t.Field {
      field_exp = None;
      field_sharp = $1;
      field_rname = $2;
      field_sep = $3;
      field_fname = $4; }
  }
  | record_exp NSIGN LIDENT LBRACE record_field_updates_opt RBRACE
  { Ast_t.Update {
      update_exp = Some $1;
      update_sharp = $2;
      update_name = $3;
      update_open = $4;
      update_assocs = $5;
      update_close = $6; }
  }
  | NSIGN LIDENT LBRACE record_field_updates_opt RBRACE
  { Ast_t.Update {
      update_exp = None;
      update_sharp = $1;
      update_name = $2;
      update_open = $3;
      update_assocs = $4;
      update_close = $5; }
  }
  | map_exp { $1 }

record_field_updates_opt:
  | record_field_updates { $1 }
  | (* empty *) { Seplist.empty }

record_field_updates:
  | rev_record_field_updates { Seplist.rev $1 }

rev_record_field_updates:
  | record_field_update { Seplist.one $1 }
  | rev_record_field_updates COMMA record_field_update
  { Seplist.cons $3 ~sep:$2 $1 }

record_field_update:
  | LIDENT EQ exp
  { { Ast_t.assoc_key = $1;
        assoc_val = $3;
        assoc_sep = $2; }
  }

map_exp:
  | NSIGN LBRACE RBRACE
  { Ast_t.Map {
      map_exp = None;
      map_nsign = $1;
      map_open = $2;
      map_pairs = None;
      map_close = $3;
    }
  }
  | record_exp NSIGN LBRACE RBRACE
  { Ast_t.Map {
      map_exp = Some $1;
      map_nsign = $2;
      map_open = $3;
      map_pairs = None;
      map_close = $4;
    }
  }
  | NSIGN LBRACE map_pairs RBRACE
  { Ast_t.Map {
      map_exp = None;
      map_nsign = $1;
      map_open = $2;
      map_pairs = Some $3;
      map_close = $4;
    }
  }
  | record_exp NSIGN LBRACE map_pairs RBRACE
  { Ast_t.Map {
      map_exp = Some $1;
      map_nsign = $2;
      map_open = $3;
      map_pairs = Some $4;
      map_close = $5;
    }
  }
  | app_exp { $1 }

map_pairs:
  | rev_map_pairs { $1 }

rev_map_pairs:
  | map_pair { Seplist.one $1 }
  | rev_map_pairs COMMA map_pair { Seplist.cons $3 ~sep:$2 $1 }

map_pair:
  | exp CEQ exp
  { { Ast_t.map_pair_key = $1;
        map_pair_op = `Update $2;
        map_pair_value = $3;
    }
  }
  | exp RARROW2 exp
  { { Ast_t.map_pair_key = $1;
        map_pair_op = `New $2;
        map_pair_value = $3;
    }
  }

app_exp:
  | primary_exp LPAREN exps_opt RPAREN
  { Ast_t.Call {
      call_fname = Ast.simple_fun_name $1;
      call_open = $2;
      call_args = $3;
      call_close = $4; }
  }
  | primary_exp COLON primary_exp LPAREN exps_opt RPAREN
  { let fname = {
      Ast_t.fun_name_mname = Some $1;
      fun_name_colon = Some $2;
      fun_name_fname = $3; }
    in
    Ast_t.Call {
      call_fname = fname;
      call_open = $4;
      call_args = $5;
      call_close = $6; }
  }
  | primary_exp { $1 }

primary_exp:
  | macro { $1 }
  | var { $1 }
  | atomic { $1 }
  | binary { $1 }
  | binary_compr { $1 }
  | tuple_skel { $1 }
  | list_skel { $1 }
  | list_compr { $1 }
  | block_exp { $1 }
  | if_exp { $1 }
  | case_exp { $1 }
  | receive_exp { $1 }
  | fun_exp { $1 }
  | try_exp { $1 }
  | LPAREN exp RPAREN { paren $1 $2 $3 }

macro:
  | Q macro_name
  { Ast_t.Macro { macro_q = $1; macro_name = $2 } }

macro_name:
  | UIDENT { $1 }
  | LIDENT { $1 }
  | ATOM { $1 }

var:
  | UIDENT { Ast_t.Var $1 }
  | USCORE { Ast_t.Uscore $1 }

atomic:
  | atom { $1 }
  | char { $1 }
  | string { $1 }
  | integer { $1 }
  | float { $1 }

atom:
  | raw_atom { Ast_t.Atom $1 }

raw_atom:
  | LIDENT { `Unenclosed $1 }
  | ATOM { `Enclosed $1 }

char:
  | CHAR { (Ast_t.Char $1) }

string:
  | rev_strvals { Ast_t.String $1 }

rev_strvals:
  | STRING { [$1] }
  | rev_strvals STRING { $2 :: $1 }

integer:
  | INT { (Ast_t.Int $1) }

float:
  | FLOAT { (Ast_t.Float $1) }

binary:
  | DGT DLT
  { Ast_t.Binary (Ast.enclose $1 Seplist.empty $2) }
  | DGT binary_elts DLT
  { Ast_t.Binary (Ast.enclose $1 $2 $3) }

binary_elts:
  | rev_binary_elts { Seplist.rev $1 }

rev_binary_elts:
  | binary_elt { Seplist.one $1 }
  | rev_binary_elts COMMA binary_elt { Seplist.cons $3 ~sep:$2 $1 }

binary_elt:
  | binary_value
  { Ast_t.(Binary_elt {
      bin_elt_val = $1;
      bin_elt_colon = None;
      bin_elt_size = None;
      bin_elt_slash = None;
      bin_elt_type = None; })
  }
  | binary_value COLON bit_size_exp
  { Ast_t.(Binary_elt {
      bin_elt_val = $1;
      bin_elt_colon = Some $2;
      bin_elt_size = Some $3;
      bin_elt_slash = None;
      bin_elt_type = None; })
  }
  | binary_value COLON bit_size_exp DIV binary_type_spec
  { Ast_t.(Binary_elt {
      bin_elt_val = $1;
      bin_elt_colon = Some $2;
      bin_elt_size = Some $3;
      bin_elt_slash = Some $4;
      bin_elt_type = Some $5; })
  }
  | binary_value DIV binary_type_spec
  { Ast_t.(Binary_elt {
      bin_elt_val = $1;
      bin_elt_colon = None;
      bin_elt_size = None;
      bin_elt_slash = Some $2;
      bin_elt_type = Some $3; })
  }

binary_value:
  | primary_exp { $1 }

bit_size_exp:
  | LPAREN bit_size_exp RPAREN { $2 }
  | LPAREN bit_size_binexp RPAREN { $2 }
  | bit_size_primary { $1 }

bit_size_binexp:
  | bit_size_exp PLUS bit_size_exp
  { binexp $1 (create $2 Ast_t.Op_add) $3 }
  | bit_size_exp MINUS bit_size_exp
  { binexp $1 (create $2 Ast_t.Op_sub) $3 }

bit_size_primary:
  | UIDENT { Ast_t.Var $1 }
  | INT { Ast_t.Int $1 }

binary_type_spec:
  | LIDENT { $1 }
  | BIT_TYPE { $1 }

binary_compr:
  | DGT binary DBAR binary_compr_quals DLT
  { Ast_t.Binary_compr {
      compr_open = $1;
      compr_exp = $2;
      compr_sep = $3;
      compr_quals = $4;
      compr_close = $5; }
  }

binary_compr_quals:
  | rev_binary_compr_quals { Seplist.rev $1 }

rev_binary_compr_quals:
  | binary_compr_qual { Seplist.one $1 }
  | rev_binary_compr_quals COMMA binary_compr_qual
  { Seplist.cons $3 ~sep:$2 $1 }

binary_compr_qual:
  | list_compr_gen { $1 }
  | binary_compr_gen { $1 }
  | binary_compr_filter { $1 }

binary_compr_gen:
  | pattern LARROW2 exp
  { Ast_t.Binary_compr_gen {
      bin_gen_ptn = $1;
      bin_gen_arrow = $2;
      bin_gen_exp = $3 }
  }

binary_compr_filter:
  | exp { $1 }

tuple_skel:
  | LBRACE exps_opt RBRACE
  { Ast_t.(Tuple (Ast.enclose $1 $2 $3)) }

list_skel:
  | LBRACK RBRACK
  { Ast_t.(List {
      list_open = $1;
      list_head = Seplist.empty;
      list_bar = None;
      list_tail = None;
      list_close = $2 })
  }
  | LBRACK exps RBRACK
  { Ast_t.(List {
      list_open = $1;
      list_head = $2;
      list_bar = None;
      list_tail = None;
      list_close = $3 })
  }
  | LBRACK exps BAR exp RBRACK
  { Ast_t.(List {
      list_open = $1;
      list_head = $2;
      list_bar = Some $3;
      list_tail = Some $4;
      list_close = $5 })
  }

list_compr:
  | LBRACK exp DBAR list_compr_quals RBRACK
  { Ast_t.List_compr {
      compr_open = $1;
      compr_exp = $2;
      compr_sep = $3;
      compr_quals = $4;
      compr_close = $5; }
  }

list_compr_quals:
  | rev_list_compr_quals { Seplist.rev $1 }

rev_list_compr_quals:
  | list_compr_qual { Seplist.one $1 }
  | rev_list_compr_quals COMMA list_compr_qual
  { Seplist.cons $3 ~sep:$2 $1 }

list_compr_qual:
  | list_compr_gen { $1 }
  | list_compr_filter { $1 }

list_compr_gen:
  | pattern LARROW exp
  { Ast_t.List_compr_gen {
      gen_ptn = $1;
      gen_arrow = $2;
      gen_exp = $3 }
  }

list_compr_filter:
  | exp { $1 }

block_exp:
  | BEGIN body END
  { Ast_t.Block (Ast.enclose $1 $2 $3) }

if_exp:
  | IF if_clauses END
  { (Ast_t.If {
      if_begin = $1;
      if_clauses = $2;
      if_end = $3 })
  }

if_clauses:
  | rev_if_clauses { Seplist.rev $1 }

rev_if_clauses:
  | if_clause { Seplist.one $1 }
  | rev_if_clauses SEMI if_clause { Seplist.cons $3 ~sep:$2 $1 }

if_clause:
  | guard RARROW body
  { { Ast_t.if_clause_guard = $1;
        if_clause_arrow = $2;
        if_clause_body = $3; }
  }

case_exp:
  | CASE exp OF cr_clauses END
  { (Ast_t.Case {
      case_begin = $1;
      case_exp = $2;
      case_of = $3;
      case_clauses = $4;
      case_end = $5; })
  }

cr_clauses:
  | rev_cr_clauses { Seplist.rev $1 }

rev_cr_clauses:
  | cr_clause { Seplist.one $1 }
  | rev_cr_clauses SEMI cr_clause { Seplist.cons $3 ~sep:$2 $1 }

cr_clause:
  | pattern RARROW body
  { { Ast_t.cr_clause_ptn = $1;
        Ast_t.cr_clause_when = None;
        Ast_t.cr_clause_guard = None;
        Ast_t.cr_clause_arrow = $2;
        Ast_t.cr_clause_body = $3; } }

  | pattern WHEN guard RARROW body
  { { Ast_t.cr_clause_ptn = $1;
        Ast_t.cr_clause_when = Some $2;
        Ast_t.cr_clause_guard = Some $3;
        Ast_t.cr_clause_arrow = $4;
        Ast_t.cr_clause_body = $5; } }

patterns:
  | rev_patterns { Seplist.rev $1 }

rev_patterns:
  | pattern { Seplist.one $1 }
  | rev_patterns COMMA pattern { Seplist.cons $3 ~sep:$2 $1 }

patterns_opt:
  | patterns { $1 }
  | (* empty *) { Seplist.empty }

pattern:
  | match_exp { $1 }

receive_exp:
  | RECEIVE cr_clauses END
  { Ast_t.Recv {
      recv_begin = $1;
      recv_clauses = $2;
      recv_after = None;
      recv_end = $3; }
  }
  | RECEIVE AFTER exp RARROW body END
  { Ast_t.Recv {
      recv_begin = $1;
      recv_clauses = Seplist.empty;
      recv_after = Some {
        recv_after_begin = $2;
        recv_after_timer = $3;
        recv_after_arrow = $4;
        recv_after_body = $5;
      };
      recv_end = $6; }
  }
  | RECEIVE cr_clauses AFTER exp RARROW body END
  { Ast_t.Recv {
      recv_begin = $1;
      recv_clauses = $2;
      recv_after = Some {
        recv_after_begin = $3;
        recv_after_timer = $4;
        recv_after_arrow = $5;
        recv_after_body = $6;
      };
      recv_end = $7; }
  }

fun_exp:
  | FUN atom COLON atom_or_var DIV integer_or_var
  { Ast_t.Module_fun {
      module_fun_prefix = $1;
      module_fun_mname = Some $2;
      module_fun_colon = Some $3;
      module_fun_fname = $4;
      module_fun_slash = $5;
      module_fun_arity = $6; }
  }
  | FUN atom DIV integer_or_var
  { Ast_t.Module_fun {
      module_fun_prefix = $1;
      module_fun_mname = None;
      module_fun_colon = None;
      module_fun_fname = $2;
      module_fun_slash = $3;
      module_fun_arity = $4; }
  }
  | FUN fun_clauses END
  { Ast_t.Anon_fun {
      anon_fun_begin = $1;
      anon_fun_body = $2;
      anon_fun_end = $3; }
  }

atom_or_var:
  | atom { $1 }
  | var { $1 }

integer_or_var:
  | integer { $1 }
  | var { $1 }

try_exp:
  | TRY seq OF cr_clauses try_catch
  { Ast_t.Try {
      try_begin = $1;
      try_exps = $2;
      try_of = Some $3;
      try_clauses = Some $4;
      try_catch = $5;
    }
  }
  | TRY seq try_catch
  { Ast_t.Try {
      try_begin = $1;
      try_exps = $2;
      try_of = None;
      try_clauses = None;
      try_catch = $3;
    }
  }

try_catch:
  | CATCH try_clauses END
  { { Ast_t.try_catch_begin = Some $1;
        try_catch_clauses = Some $2;
        try_catch_after = None;
        try_catch_end = $3; }
  }
  | CATCH try_clauses AFTER seq END
  { { Ast_t.try_catch_begin = Some $1;
        try_catch_clauses = Some $2;
        try_catch_after = Some {
            try_catch_after_begin = $3;
            try_catch_after_exps = $4; };
        try_catch_end = $5; }
  }
  | AFTER seq END
  { { Ast_t.try_catch_begin = None;
        try_catch_clauses = None;
        try_catch_after = Some {
            try_catch_after_begin = $1;
            try_catch_after_exps = $2; };
        try_catch_end = $3; }
  }

try_clauses:
  | rev_try_clauses { Seplist.rev $1 }

rev_try_clauses:
  | try_clause { Seplist.one $1 }
  | rev_try_clauses SEMI try_clause
  { Seplist.cons $3 ~sep:$2 $1 }

try_clause:
  | primary_exp RARROW body
  { { Ast_t.try_clause_cls = $1;
        try_clause_exn = None;
        try_clause_stack = None;
        try_clause_guard = None;
        try_clause_arrow = $2;
        try_clause_body = $3; }
  }
  | primary_exp WHEN guard RARROW body
  { { Ast_t.try_clause_cls = $1;
        try_clause_exn = None;
        try_clause_stack = None;
        try_clause_guard = Some ($2, $3);
        try_clause_arrow = $4;
        try_clause_body = $5; }
  }
  | primary_exp COLON primary_exp RARROW body
  { { Ast_t.try_clause_cls = $1;
        try_clause_exn = Some ($2, $3);
        try_clause_stack = None;
        try_clause_guard = None;
        try_clause_arrow = $4;
        try_clause_body = $5; }
  }
  | primary_exp COLON primary_exp WHEN guard RARROW body
  { { Ast_t.try_clause_cls = $1;
        try_clause_exn = Some ($2, $3);
        try_clause_stack = None;
        try_clause_guard = Some ($4, $5);
        try_clause_arrow = $6;
        try_clause_body = $7; }
  }
  | primary_exp COLON primary_exp COLON primary_exp RARROW body
  { { Ast_t.try_clause_cls = $1;
        try_clause_exn = Some ($2, $3);
        try_clause_stack = Some ($4, $5);
        try_clause_guard = None;
        try_clause_arrow = $6;
        try_clause_body = $7; }
  }
  | primary_exp COLON primary_exp COLON primary_exp WHEN guard RARROW body
  { { Ast_t.try_clause_cls = $1;
        try_clause_exn = Some ($2, $3);
        try_clause_stack = Some ($4, $5);
        try_clause_guard = Some ($6, $7);
        try_clause_arrow = $8;
        try_clause_body = $9; }
  }
