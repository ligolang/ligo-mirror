open Simple_utils.Trace
open Test_helpers
open Main_errors

let () = Unix.putenv ~key:"LIGO_FORCE_NEW_TYPER" ~data:"false"
type syntax = string
type group_name = string
type lang = Meta | Object (* Object = normal LIGO code ; Meta = ligo test framework code *)
module SnippetsGroup = Caml.Map.Make(struct type t = (syntax * group_name * Environment.Protocols.t) let compare a b = Caml.compare a b end)


type snippetsmap = (lang * string) SnippetsGroup.t

let arg_to_string x =
  match x with
  | Md.Field s -> s
  | Md.NameValue (k,v) -> Format.asprintf "%s=%s" k v
let get_proto p =
  let opt = try Environment.Protocols.protocols_to_variant p with
    _ -> None
  in
  match opt with
  | Some x -> x
  | None -> failwith "unknown protocol"
let current_proto = get_proto "current"
(*
  Binds the snippets by (syntax, group_name).
  Syntax and group_name being retrieved from the .md file header & arguments
  e.g. in the .md file:
    ```syntax group=group_name
      <some code>
    ```
*)
let get_groups md_file : snippetsmap =
  let channel = In_channel.create md_file in
  let lexbuf = Lexing.from_channel channel in
  let code_blocks = Md.token lexbuf in
  let aux : snippetsmap -> Md.block -> snippetsmap =
    fun grp_map el ->
      match el.header  with
      | Some ("pascaligo" as s) | Some ("cameligo" as s) | Some ("reasonligo" as s) | Some ("jsligo" as s) -> (
        let () = (*sanity check*)
          List.iter ~f:
            (fun arg ->
              match arg with
              | Md.Field "" | Md.Field "skip" | Md.NameValue ("group",_) | Md.Field "test-ligo" | Md.NameValue ("protocol",_) -> ()
              | Md.Field _ | Md.NameValue (_,_) ->
                failwith (
                  Format.asprintf "unknown argument '%s' in code block at line %d of file %s" (arg_to_string arg) el.line el.file
                )
            )
            el.arguments
        in
        match el.arguments with
        | [Md.Field ""] -> (
          SnippetsGroup.update (s,"ungrouped",current_proto)
            (fun arg_content ->
              match arg_content with
              | Some (lang,ct) -> Some (lang, String.concat ~sep:"\n" (ct::el.contents))
              | None -> Some (Object, String.concat ~sep:"\n" el.contents)
            )
            grp_map
        )
        | [Md.Field "skip"] -> grp_map
        | [Md.Field "test-ligo" ; Md.NameValue ("group", name) ; Md.NameValue ("protocol",x)] -> (
          let lang = Meta in
          SnippetsGroup.update (s,name,get_proto x)
            (fun arg_content ->
              match arg_content with
              | Some (lang',ct) when Caml.(=) lang lang' -> Some (lang, String.concat ~sep:"\n" (ct::el.contents))
              | _ -> Some (lang, String.concat ~sep:"\n" el.contents)
            )
            grp_map
        )
        | [Md.Field "test-ligo" ; Md.NameValue ("group", name)] -> (
          let lang = Meta in
          SnippetsGroup.update (s,name,current_proto)
            (fun arg_content ->
              match arg_content with
              | Some (lang',ct) when Caml.(=) lang lang' -> Some (lang, String.concat ~sep:"\n" (ct::el.contents))
              | _ -> Some (lang, String.concat ~sep:"\n" el.contents)
            )
            grp_map
        )
        | [Md.NameValue ("group", name); Md.NameValue ("protocol",x)] -> (
          let lang = Object in
          SnippetsGroup.update (s,name,get_proto x)
            (fun arg_content ->
              match arg_content with
              | Some (lang',ct) when Caml.(=) lang lang' -> Some (lang, String.concat ~sep:"\n" (ct::el.contents))
              | _ -> Some (lang, String.concat ~sep:"\n" el.contents)
            )
            grp_map
        )
        | [Md.NameValue ("group", name)] -> (
          let lang = Object in
          SnippetsGroup.update (s,name,current_proto)
            (fun arg_content ->
              match arg_content with
              | Some (lang',ct) when Caml.(=) lang lang' -> Some (lang, String.concat ~sep:"\n" (ct::el.contents))
              | _ -> Some (lang, String.concat ~sep:"\n" el.contents)
            )
            grp_map
        )
        | args ->
          let () = List.iter ~f: (function Md.NameValue (x,y) -> Format.printf "NamedValue %s %s\n" x y | Md.Field x -> Format.printf "%s\n" x) args in
          failwith "Block arguments (above) not supported"
      )
      | None | Some _ -> grp_map
  in
  List.fold_left ~f:aux ~init:SnippetsGroup.empty code_blocks

(**
  if Meta : evaluate each expression in each programs from the snippets group map
  if Object : run the ligo test framework
**)
let compile_groups ~raise filename grp_list =
  let add_warning _ = () in
  let aux : (syntax * group_name * Environment.Protocols.t) * (lang * string) -> unit =
    fun ((syntax , grp, protocol_version) , (lang , contents)) ->
      trace ~raise (test_md_file filename syntax grp contents) @@
      fun ~raise ->
      let options    = Compiler_options.make ~raw_options:Compiler_options.default_raw_options ~protocol_version () in
      let syntax     = Syntax.of_string_opt ~raise (Syntax_name syntax) (Some filename) in
      let meta       = Ligo_compile.Of_source.make_meta syntax in
      let c_unit,_   = Ligo_compile.Of_source.compile_string ~raise ~options:options.frontend ~meta contents in
      let imperative = Ligo_compile.Of_c_unit.compile ~raise ~add_warning ~meta c_unit filename in
      let sugar      = Ligo_compile.Of_imperative.compile ~raise imperative in
      let core       = Ligo_compile.Of_sugar.compile sugar in
      let stdlib     = Build.Stdlib.core ~options meta.syntax in
      match lang with
      | Meta ->
        let init_env = Environment.default_with_test protocol_version in
        let options = Compiler_options.set_init_env options init_env in
        let options = Compiler_options.set_test_flag options true in
        let testlib    = Build.Testlib.core ~options meta.syntax in
        let core = testlib @ stdlib @ core in
        let typed   = Ligo_compile.Of_core.typecheck ~raise ~add_warning ~options Env core in
        let _ = Interpreter.eval_test ~options ~raise ~add_warning ~steps:5000 typed in
        ()
      | Object ->
        let core = stdlib @ core in
        let typed     = Ligo_compile.Of_core.typecheck ~raise ~add_warning ~options Env core in
        let agg_prg   = Ligo_compile.Of_typed.compile_program ~raise typed in
        let aggregated_with_unit = Ligo_compile.Of_typed.compile_expression_in_context ~raise ~add_warning ~options:options.middle_end (Ast_typed.e_a_unit ()) agg_prg in
        let mini_c = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated_with_unit in
        let _michelson : Stacking__Compiler_program.compiled_expression = Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c in
        ()
  in
  let () = List.iter ~f:aux grp_list in
  ()

let compile ~raise filename () =
  let groups = get_groups filename in
  let groups_map = SnippetsGroup.bindings groups in
  let () = compile_groups ~raise filename groups_map in
  ()

let get_all_md_files () =
  let exclude_files = [
    "./gitlab-pages/docs/demo/ligo-snippet.md" ;
  ] in
  let ic = Unix.open_process_in "find ./gitlab-pages/docs -iname \"*.md\"" in
  let all_input = ref [] in
  let () =
    try
      while true do
        let md_file = In_channel.input_line_exn ic in
        if not (List.exists ~f:(String.equal md_file) exclude_files) then
          let grp = get_groups md_file in
          if not (SnippetsGroup.is_empty grp) then
            all_input := md_file :: !all_input
      done
    with
      End_of_file ->
      In_channel.close ic
  in
  !all_input

let main =
  Sys.chdir "../.." ;
  test_suite "Markdown files" @@
    List.map
      ~f:(fun md_file ->
        let test_name = "File : "^md_file^"\"" in
        test test_name (compile md_file)
      )
      (get_all_md_files ())
