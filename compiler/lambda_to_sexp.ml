open Core
open Common
open Located
open Lambda_t
open Lambda0

let rec to_sexp = function
  | Event ev ->
    let loc = Printf.sprintf "%d:%d-%d:%d"
        (ev.ev_loc.start.line + 1)
        ev.ev_loc.start.col
        (ev.ev_loc.end_.line + 1)
        ev.ev_loc.end_.col
    in
    let exp = to_sexp ev.ev_exp in
    let body = [Sexp.Atom loc; exp] in
    begin match ev.ev_kind with
      | Ev_before -> Sexp.tagged "before" body
      | Ev_after _ -> Sexp.tagged "after" body
      | Ev_fun -> Sexp.tagged "fun_body" body
    end
  | Module m ->
    let attr name value =
      Sexp.List [Sexp.Atom name; value]
    in
    Sexp.tagged "module"
      (List.concat [
          [attr "name" (Sexp.Atom (Option.value_exn m.mod_name))];
          [attr "authors"
             (Sexp.List (List.map m.mod_authors
                           ~f:(fun auth -> Sexp.Atom auth)))];
          Option.value_map m.mod_behav
            ~default:[]
            ~f:(fun name -> [attr "behav" (Sexp.Atom name)]);
          [attr "exports"
             (Sexp.List (List.map m.mod_exports
                           ~f:(fun (name, arity) ->
                               Sexp.Atom (sprintf "%s/%d" name arity))))];
          [to_sexp m.mod_code]
        ])
  | Let (binds, exp) ->
    let binds1 =
      List.map binds
        ~f:(fun (l, r) ->
            Sexp.List [Sexp.Atom l; to_sexp r])
    in
    Sexp.tagged "let" [Sexp.List binds1; to_sexp exp]
  | Fun (name, params, body) ->
    Sexp.tagged "fun" [
      Sexp.Atom (Option.value name ~default:"<none>");
      Sexp.List (List.map params ~f:(fun id -> Sexp.Atom id));
      to_sexp body]
  | Clos (name, params, body) ->
    Sexp.tagged "clos" [
      Sexp.Atom (Option.value name ~default:"<none>");
      Sexp.List (List.map params ~f:(fun id -> Sexp.Atom id));
      to_sexp body]
  | Fun_sig (name, arity) ->
    Sexp.tagged "fun_sig" [Sexp.Atom name; Sexp.Atom (Int.to_string arity)]
  | Seq (exp1, exp2) ->
    Sexp.tagged "seq" [to_sexp exp1; to_sexp exp2]
  | Apply (f, args) ->
    Sexp.tagged "apply" (to_sexp f :: List.map args ~f:to_sexp)
  | Spawn (f, args) ->
    Sexp.tagged "spawn" [to_sexp f; to_sexp args]
  | If (cond, then_, else_) ->
    Sexp.tagged "if" [to_sexp cond; to_sexp then_; to_sexp else_]
  | Switch _ ->
    (* TODO *)
    Sexp.Atom "switch"
  | Catch (catch, with_) ->
    let ns, exps = List.fold_left with_
        ~init:([], [])
        ~f:(fun (ns, exps) (n, exp) ->
            Sexp.Atom (Int.to_string (n + 1)) :: ns,
            to_sexp exp :: exps) in
    Sexp.tagged "catch"
      (List.append [to_sexp catch;
                    Sexp.Atom "with";
                    Sexp.List ns] exps)
  | Exit n ->
    Sexp.tagged "exit" [Sexp.Atom (Int.to_string (n + 1))]
  | No_match ->
    Sexp.Atom "no_match"
  | Try (exp, id, action) ->
    Sexp.tagged "try" [Sexp.Atom id; to_sexp action]
  | Loop (cond, body) ->
    Sexp.tagged "loop" [to_sexp cond; to_sexp body]
  | Create_block (tag, elts) ->
    Sexp.tagged "create_block"
      [Sexp.Atom (Block_tag.to_string tag);
       Sexp.List (List.map elts ~f:to_sexp)]
  | Local name -> Sexp.Atom ("$" ^ name)
  | Get_module -> Sexp.Atom "getmodule"
  | Get_field (map, idx) ->
    Sexp.tagged "get_field" [to_sexp map; to_sexp idx]
  | Get_global key ->
    Sexp.tagged "get_global" [to_sexp key]
  | Get_rec (exp, rname, fname) ->
    Sexp.tagged "get_record" [
      opt_to_sexp exp; Sexp.Atom rname; Sexp.Atom fname]
  | Get_bitstr (value, spec, pos) ->
    Sexp.tagged "get_bitstr" [to_sexp value;
                              Sexp.Atom (Bitstr.Repr.spec_to_string spec);
                              to_sexp pos]
  | Set_local (name, value) ->
    Sexp.tagged "set_local" [Atom name; to_sexp value]
  | Set_global (key, value) ->
    Sexp.tagged "set_global" [to_sexp key; to_sexp value]
  | Update_rec up ->
    let assocs = 
      List.map up.up_assocs
        ~f:(fun (key, value) -> 
            Sexp.List [Sexp.Atom key; to_sexp value]) in
    Sexp.tagged "update"
      (List.append [opt_to_sexp up.up_exp; Sexp.Atom up.up_name] assocs)
  | Not exp ->
    Sexp.tagged "not" [to_sexp exp]
  | And (a, b) ->
    Sexp.tagged "&&" [to_sexp a; to_sexp b]
  | Or (a, b) ->
    Sexp.tagged "||" [to_sexp a; to_sexp b]
  | Eq (a, b) ->
    Sexp.tagged "=" [to_sexp a; to_sexp b]
  | Ne (a, b) ->
    Sexp.tagged "!=" [to_sexp a; to_sexp b]
  | Add (a, b) ->
    Sexp.tagged "+" [to_sexp a; to_sexp b]
  | Sub (a, b) ->
    Sexp.tagged "-" [to_sexp a; to_sexp b]
  | Mul (a, b) ->
    Sexp.tagged "*" [to_sexp a; to_sexp b]
  | Div (a, b) ->
    Sexp.tagged "/" [to_sexp a; to_sexp b]
  | Rem (a, b) ->
    Sexp.tagged "rem" [to_sexp a; to_sexp b]
  | Quo (a, b) ->
    Sexp.tagged "div" [to_sexp a; to_sexp b]
  | Atom name ->
    Sexp.Atom (name ^ "!")
  | Bool true -> Sexp.Atom "true"
  | Bool false -> Sexp.Atom "false"
  | String s -> Sexp.tagged "string" [String.sexp_of_t s]
  | Int s -> String.sexp_of_t s
  | Float s -> String.sexp_of_t s
  | Block (tag, elts) ->
    Sexp.tagged "block"
      [Sexp.Atom (Block_tag.to_string tag);
       Sexp.List (List.map elts ~f:to_sexp)]
  | Bitstr bits ->
    Sexp.tagged "bitstr" (bits_to_sexp bits)
  | Create_bitstr bits ->
    Sexp.tagged "create_bitstr" (bits_to_sexp bits)
  | Block_size exp ->
    Sexp.tagged "block_size" [to_sexp exp]
  | Block_first exp ->
    Sexp.tagged "block_first" [to_sexp exp]
  | Block_drop (exp, n) ->
    Sexp.tagged "block_drop" [to_sexp exp; Sexp.Atom (Int.to_string n)]
  | List_cons (a, b) ->
    Sexp.tagged "[|]" [to_sexp a; to_sexp b]
  | List_concat (a, b) ->
    Sexp.tagged "++" [to_sexp a; to_sexp b]
  | List_sub (a, b) ->
    Sexp.tagged "--" [to_sexp a; to_sexp b]
  | Ok [] ->
    Sexp.Atom "ok"
  | Ok exps ->
    Sexp.tagged "ok" (list_to_sexp exps)
  | Error [] ->
    Sexp.Atom "error"
  | Error exps ->
    Sexp.tagged "error" (list_to_sexp exps)
  | Test_atom exp ->
    Sexp.tagged "testatom" [to_sexp exp]
  | Test_binary exp ->
    Sexp.tagged "test_binary" [to_sexp exp]
  | Test_bitstr exp ->
    Sexp.tagged "test_bitstr" [to_sexp exp]
  | Test_bool exp ->
    Sexp.tagged "test_bool" [to_sexp exp]
  | Test_float exp ->
    Sexp.tagged "test_float" [to_sexp exp]
  | Test_fun1 exp ->
    Sexp.tagged "test_fun1" [to_sexp exp]
  | Test_fun2 (exp1, exp2) ->
    Sexp.tagged "test_fun2" [to_sexp exp1; to_sexp exp2]
  | Test_int exp ->
    Sexp.tagged "test_int" [to_sexp exp]
  | Test_list exp ->
    Sexp.tagged "test_list" [to_sexp exp]
  | Test_number exp ->
    Sexp.tagged "test_number" [to_sexp exp]
  | Test_pid exp ->
    Sexp.tagged "test_pid" [to_sexp exp]
  | Test_port exp ->
    Sexp.tagged "test_port" [to_sexp exp]
  | Test_ref exp ->
    Sexp.tagged "test_ref" [to_sexp exp]
  | Test_record2 (exp1, exp2) ->
    Sexp.tagged "test_record2" [to_sexp exp1; to_sexp exp2]
  | Test_record3 (exp1, exp2, exp3) ->
    Sexp.tagged "test_record3" [to_sexp exp1; to_sexp exp2; to_sexp exp3]
  | Test_tuple exp ->
    Sexp.tagged "test_tuple" [to_sexp exp]
  | Test_nonnil exp ->
    Sexp.tagged "test_nonnil" [to_sexp exp]
  | Self -> Sexp.Atom "self"
  (* TODO: others *)
  | Nop -> Sexp.Atom "nop"
  | _ -> Sexp.Atom "<?>"

and opt_to_sexp exp =
  Option.value_map exp
    ~default:(Sexp.Atom "<none>")
    ~f:to_sexp

and list_to_sexp exps =
  List.map exps ~f:to_sexp

and bits_to_sexp (bits : (t, t) Bitstr.t) =
  let open Bitstr.Repr in
  [
    to_sexp bits.value;
    to_sexp bits.size;
    Sexp.Atom (Bitstr.Repr.spec_to_string bits.spec)
  ]

let to_string t =
  Xsexp.to_string t ~f:to_sexp
