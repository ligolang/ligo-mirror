open Trace

(* Helpers *)

let variant_to_syntax (v: Ligo_compile.Helpers.v_syntax) =
  match v with
  | PascaLIGO -> "pascaligo"
  | CameLIGO -> "cameligo"
  | ReasonLIGO -> "reasonligo"
  | JsLIGO -> "jsligo"

let get_declarations_core core_prg =
     let func_declarations  = Ligo_compile.Of_core.list_declarations core_prg in
     let type_declarations  = Ligo_compile.Of_core.list_type_declarations core_prg in
     let mod_declarations  = Ligo_compile.Of_core.list_mod_declarations core_prg in
     func_declarations @ type_declarations @ mod_declarations

let get_declarations_typed typed_prg =
     let func_declarations  = Ligo_compile.Of_typed.list_declarations typed_prg in
     let type_declarations  = Ligo_compile.Of_typed.list_type_declarations typed_prg in
     let mod_declarations  = Ligo_compile.Of_typed.list_mod_declarations typed_prg in
     func_declarations @ type_declarations @ mod_declarations

(* Error and warnings *)

let add_warning _ = ()

(* REPL logic *)

type repl_result =
    Expression_value of Ast_core.expression
  | Defined_values_core of Ast_core.module_
  | Defined_values_typed of Ast_typed.module'
  | Just_ok

open Display

let repl_result_ppformat ~display_format f = function
    Expression_value expr ->
     (match display_format with
      | Human_readable | Dev -> Ast_core.PP.expression f expr)
  | Defined_values_core module_ ->
     (match display_format with
      | Human_readable | Dev -> Simple_utils.PP_helpers.list_sep_d
                                  Simple_utils.PP_helpers.string f
                                  (get_declarations_core module_))
  | Defined_values_typed module' ->
     (match display_format with
      | Human_readable | Dev -> Simple_utils.PP_helpers.list_sep_d
                                  Simple_utils.PP_helpers.string f
                                  (get_declarations_typed module'))
  | Just_ok -> Simple_utils.PP_helpers.string f "Done."

let repl_result_jsonformat = function
    Expression_value expr ->
     let value = Format.asprintf "%a" Ast_core.PP.expression expr in
     `Assoc [("value", `String value)]
  | Defined_values_core module_ ->
     let func_declarations  = Ligo_compile.Of_core.list_declarations module_ in
     let type_declarations  = Ligo_compile.Of_core.list_type_declarations module_ in
     let name n = `Assoc [("name", `String n)] in
     let defs = List.map ~f:name (func_declarations @ type_declarations) in
     `Assoc [("definitions", `List defs)]
  | Defined_values_typed module' ->
     let func_declarations  = Ligo_compile.Of_typed.list_declarations module' in
     let type_declarations  = Ligo_compile.Of_typed.list_type_declarations module' in
     let name n = `Assoc [("name", `String n)] in
     let defs = List.map ~f:name (func_declarations @ type_declarations) in
     `Assoc [("definitions", `List defs)]
  | Just_ok -> `Assoc []

let repl_result_format : 'a Display.format = {
    pp = repl_result_ppformat ;
    to_json = repl_result_jsonformat ;
}

module Run = Ligo_run.Of_michelson

type state = { env : Ast_typed.environment;
               syntax : Ligo_compile.Helpers.v_syntax;
               infer : bool ;
               protocol : Environment.Protocols.t;
               top_level : Ast_typed.module_fully_typed;
               dry_run_opts : Run.options;
              }

let try_eval ~raise state s =
  let options = Compiler_options.make ~infer:state.infer ~protocol_version:state.protocol () in
  let options = {options with init_env = state.env } in
  let typed_exp,env = Ligo_compile.Utils.type_expression_string ~raise ~options:options state.syntax s state.env in
  let aggregated_exp = Ligo_compile.Of_typed.compile_expression ~raise typed_exp in
  let mini_c = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated_exp in
  let compiled_exp = Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c in
  let options = state.dry_run_opts in
  let runres = Run.run_expression ~raise ~options:options compiled_exp.expr compiled_exp.expr_ty in
  let x = Decompile.Of_michelson.decompile_expression ~raise aggregated_exp.type_expression runres in
  match x with
  | Success expr ->
     let state = { state with env = env; top_level = state.top_level } in
     (state, Expression_value expr)
  | Fail _ ->
    raise.raise `Repl_unexpected

let concat_modules ~statement (m1 : Ast_typed.module_fully_typed) (m2 : Ast_typed.module_fully_typed) : Ast_typed.module_fully_typed =
  let (Module_Fully_Typed m1) = m1 in
  let (Module_Fully_Typed m2) = m2 in
  let () = if statement then assert (List.length m2 = 1) in
  Module_Fully_Typed (m1 @ m2)

let try_statement ~raise state s(* let x = 42 *) =
  let options = Compiler_options.make ~infer:state.infer ~protocol_version:state.protocol () in
  let options = {options with init_env = state.env } in
  try
    try_with (fun ~raise ->
      let typed_prg,core_prg,env =
        Ligo_compile.Utils.type_contract_string ~raise ~add_warning ~options:options state.syntax s state.env in
      let state = { state with env = env;
                               top_level = concat_modules ~statement:true state.top_level typed_prg;
                               } in
      (state, Defined_values_core core_prg))
    (function
        (`Main_parser _ : Main_errors.all)
      | (`Main_cit_jsligo _ : Main_errors.all)
      | (`Main_cit_pascaligo _ : Main_errors.all)
      | (`Main_cit_cameligo _ : Main_errors.all)
      | (`Main_cit_reasonligo _ : Main_errors.all) ->
         try_eval ~raise state s
      | e -> raise.raise e)
  with
  | Failure _ ->
     raise.raise `Repl_unexpected

let import_file ~raise state file_name module_name =
  let options = Compiler_options.make ~infer:state.infer ~protocol_version:state.protocol () in
  let options = {options with init_env = state.env } in
  let module_,env = Build.combined_contract ~raise ~add_warning ~options (variant_to_syntax state.syntax) file_name in
  let env = Ast_typed.Environment.add_module ~public:true module_name env state.env in
  let module_ = Ast_typed.(Module_Fully_Typed [Location.wrap @@ Declaration_module {module_binder=module_name;module_;module_attr={public=true}}]) in
  let state = { state with env = env; top_level = concat_modules ~statement:true state.top_level module_ } in
  (state, Just_ok)

let use_file ~raise state s =
  let options = Compiler_options.make ~infer:state.infer ~protocol_version:state.protocol () in
  let options = {options with init_env = state.env } in
  (* Missing typer environment? *)
  let module',env = Build.combined_contract ~raise ~add_warning ~options (variant_to_syntax state.syntax) s in
  let state = { state with env = env;
                           top_level = concat_modules ~statement:false state.top_level module'
                          } in
  let (Module_Fully_Typed module') = module' in
  (state, Defined_values_typed module')

(* REPL "parsing" *)

type repl_directive = Use of string
                    | Import of string * string
                    | Expr of string

let parse s =
  let whitespace = "[ \n\r\x0c\t]" in
  let re_use = "^" ^ (whitespace ^ "*") ^ "#use" ^ (whitespace ^ "+") ^ "\"\\(.*\\)\"" ^ (whitespace ^ "*") ^ "$" in
  let re_import = "^" ^ (whitespace ^ "*") ^ "#import" ^ (whitespace ^ "+") ^ "\"\\(.*\\)\"" ^ (whitespace ^ "+") ^ "\"\\(.*\\)\"" ^ (whitespace ^ "*") ^ "$" in
  if Str.(string_match (regexp re_use) s 0) then
    Use (Str.matched_group 1 s)
  else if Str.(string_match (regexp re_import) s 0)  then
    Import (Str.matched_group 1 s, Str.matched_group 2 s)
  else
    Expr s

(* REPL main and loop *)

let eval display_format state c =
  let (Ex_display_format t) = display_format in
  match Trace.to_stdlib_result c with
    Ok (state, out) ->
     let disp = (Displayable {value = out; format = repl_result_format }) in
     let out : string =
       match t with
       | Human_readable -> convert ~display_format:t disp ;
       | Dev -> convert ~display_format:t disp ;
       | Json -> Yojson.Safe.pretty_to_string @@ convert ~display_format:t disp in
     (1, state, out)
  | Error e ->
     let disp = (Displayable {value = e; format = Main_errors.Formatter.error_format }) in
     let out : string =
       match t with
       | Human_readable -> convert ~display_format:t disp ;
       | Dev -> convert ~display_format:t disp ;
       | Json -> Yojson.Safe.pretty_to_string @@ convert ~display_format:t disp in
     (0, state, out)

let parse_and_eval display_format state s =
  let c = match parse s with
    | Use s -> use_file state s
    | Import (fn, mn) -> import_file state fn mn
    | Expr s -> try_statement state s in
  eval display_format state c

let welcome_msg = "Welcome to LIGO's interpreter!
Included directives:
  #use \"file_path\";;
  #import \"file_path\" \"module_name\";;"

let make_initial_state syntax protocol infer dry_run_opts =
  { env = Environment.default protocol;
    top_level = Module_Fully_Typed [];
    syntax = syntax;
    infer = infer;
    protocol = protocol;
    dry_run_opts = dry_run_opts;
  }

let rec read_input prompt delim =
  let open Option in
  match LNoise.linenoise prompt with
  | exception Sys.Break | None -> None
  | Some s -> LNoise.history_add s |> ignore;
              let result = Str.split_delim (Str.regexp delim) s in
              match result with
              | [] | [_] ->
                 let* i = read_input "" delim in
                 some @@ s ^ "\n" ^ i
              | hd :: _ -> some @@ hd

let rec loop syntax display_format state n =
  let prompt = Format.sprintf "In  [%d]: " n in
  let s = read_input prompt ";;" in
  match s with
  | Some s ->
     let k, state, out = parse_and_eval display_format state s in
     let out = Format.sprintf "Out [%d]: %s" n out in
     print_endline out;
     loop syntax display_format state (n + k)
  | None -> ()

let main syntax display_format protocol typer_switch dry_run_opts init_file =
  print_endline welcome_msg;
  let state = make_initial_state syntax protocol typer_switch dry_run_opts in
  let state = match init_file with
    | None -> state
    | Some file_name -> let c = use_file state file_name in
                        let _, state, _ = eval (Ex_display_format Dev) state c in
                        state in
  LNoise.set_multiline true;
  loop syntax display_format state 1
