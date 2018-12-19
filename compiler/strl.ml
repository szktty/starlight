open Core
open Common

let summary = "[WIP] A yet another Erlang implementation"

let runtime_path = "./strlrun"

let file_exists file =
  match Sys.file_exists ~follow_symlinks:true file with
  | `No | `Unknown ->
    Printf.printf "Error: No such file `%s'\n" file;
    exit (-1)
  | `Yes -> ()

let parse_file file =
  In_channel.with_file file
    ~f:(fun chan ->
        let buf = Lexing.from_channel chan in
        try begin
          Parser.prog Lexer.read buf
        end with
        | Lexer.Syntax_error (pos, msg) ->
          let open Position in
          printf "Line %d, column %d: %s\n" pos.line pos.col msg;
          exit (-1)
        | Parser.Error ->
          let pos = Lexing.lexeme_start_p buf in
          printf "Line %d, column %d: Invalid syntax\n"
            pos.pos_lnum (pos.pos_cnum-pos.pos_bol+1);
          exit (-1)
        | e -> raise e)

let compile_tree file node =
  Lambda.from_node (parse_file file)

let print_parsetree node =
  printf "%s\n" (Ast.to_string node)

let print_lambda form =
  printf "%s\n" (Lambda.to_string form)

let compile_lambda form =
  Compiler.compile form

let print_code code =
  let s = Bytecode.Code.to_debug code in
  Printf.printf "%s\n" s

let compile ~parsetree ~lambda ~instr file =
  let node = parse_file file in
  begin if parsetree then
      print_parsetree node
  end;

  let form = compile_tree file node in
  begin if lambda then
      print_lambda form
  end;

  let m, code = compile_lambda form in
  begin if instr then
      print_code code
  end;

  let bc_file, bc = Bytecode.from_code file m code in
  Bytecode.write bc_file (Bytecode.to_json bc)

(* Example: ./strl build -parsetree -lambda -instr FILE *)
let cmd_build =
  Command.basic
    ~summary:"compile"
    (let open Command.Let_syntax in
     [%map_open
       let debug = flag "-d" no_arg ~doc:" debug output"
       and parsetree = flag "-parsetree" no_arg ~doc:" print parse tree"
       and lambda = flag "-lambda" no_arg ~doc:" print lambda form"
       and instr = flag "-instr" no_arg ~doc:" print instructions"
       and verbose = flag "-v" no_arg ~doc:" print verbose message"
       and file = anon (maybe ("filename" %: string))
       in
       (fun () ->
          Conf.debug_mode := debug;
          Conf.verbose_mode := verbose;
          begin match file with
            | None ->
              Printf.printf "Error: No input files";
              exit 1
            | Some file ->
              file_exists file;
              compile ~parsetree ~lambda ~instr file
          end)
     ])

let cmd_run =
  Command.basic
    ~summary:"compile and run"
    (let open Command.Let_syntax in
     [%map_open
       let debug = flag "-d" no_arg ~doc:" debug output"
       and parsetree = flag "-parsetree" no_arg ~doc:" print parse tree"
       and lambda = flag "-lambda" no_arg ~doc:" print lambda form"
       and instr = flag "-instr" no_arg ~doc:" print instructions"
       and verbose = flag "-verbose" no_arg ~doc:" print verbose message"
       and syntax = flag "-syntax" no_arg ~doc:" check syntax only"
       and runtime = flag "-runtime"
           (optional string)
           ~doc:"runtime path"
       and file = anon (maybe ("filename" %: string))
       in
       (fun () ->
          Conf.debug_mode := debug;
          Conf.verbose_mode := verbose;
          begin match file with
            | None ->
              Printf.printf "Error: No input files";
              exit 1
            | Some file ->
              file_exists file;
              compile ~parsetree ~lambda ~instr file;
              let rt_path = Option.value runtime ~default:runtime_path in
              let _ = Sys.command (Printf.sprintf "%s %s" rt_path
                                     (Bytecode.bc_filename file)) in
              ()
          end)
     ])

let main =
  Command.group
    ~summary
    [("build", cmd_build);
     ("run", cmd_run)]

let () =
  try
    Printexc.record_backtrace true;
    Command.run ~version:Conf.version main
  with
  | e -> raise e
