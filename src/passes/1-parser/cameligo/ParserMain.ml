(** Driver for the CameLIGO parser *)

let extension = ".mligo"
let options = EvalOpt.read "CameLIGO" extension

open Printf

(** Error printing and exception tracing
*)
let () = Printexc.record_backtrace true

(** Extracting the input file
*)
let file =
  match options#input with
    None | Some "-" -> false
  |         Some _  -> true

(** {1 Error printing and exception tracing} *)

let () = Printexc.record_backtrace true

let external_ text =
  Utils.highlight (sprintf "External error: %s" text); exit 1;;

(** {1 Preprocessing the input source and opening the input channels} *)

(** Path for CPP inclusions (#include)
*)
let lib_path =
  match options#libs with
      [] -> ""
  | libs -> let mk_I dir path = sprintf " -I %s%s" dir path
           in List.fold_right mk_I libs ""

let prefix =
  match options#input with
    None | Some "-" -> "temp"
  | Some file ->  Filename.(file |> basename |> remove_extension)

let suffix = ".pp" ^ extension

let pp_input =
  if Utils.String.Set.mem "cpp" options#verbose
  then prefix ^ suffix
  else let pp_input, pp_out = Filename.open_temp_file prefix suffix
       in close_out pp_out; pp_input

let cpp_cmd =
  match options#input with
    None | Some "-" ->
      sprintf "cpp -traditional-cpp%s - > %s"
                     lib_path pp_input
  | Some file ->
      sprintf "cpp -traditional-cpp%s %s > %s"
                     lib_path file pp_input

let () =
  if Utils.String.Set.mem "cpp" options#verbose
  then eprintf "%s\n%!" cpp_cmd;
  if Sys.command cpp_cmd <> 0 then
    external_ (sprintf "the command \"%s\" failed." cpp_cmd)

(** {1 Instanciating the lexer} *)

module Lexer = Lexer.Make (LexToken)
module Log = LexerLog.Make (Lexer)
module ParserFront = ParserAPI.Make (Lexer) (Parser) (ParErr)

let lexer_inst = Lexer.open_token_stream (Some pp_input)
let Lexer.{read; buffer; get_win; get_pos; get_last; close} = lexer_inst

and cout = stdout

let log = Log.output_token ~offsets:options#offsets
                           options#mode options#cmd cout

and close_all () = close (); close_out cout

(** {1 Tokeniser} *)

let tokeniser = read ~log

(** {1 Main} *)

let () =
  try
     let ast =
       if   options#mono
       then ParserFront.mono_contract tokeniser buffer
       else ParserFront.incr_contract lexer_inst in
    if Utils.String.Set.mem "ast" options#verbose
    then let buffer = Buffer.create 131 in
         let state = ParserLog.mk_state
           ~offsets:options#offsets
           ~mode:options#mode
           ~buffer in
         begin
           ParserLog.pp_ast state ast;
           Buffer.output_buffer stdout buffer
         end
    else if Utils.String.Set.mem "ast-tokens" options#verbose
    then let buffer = Buffer.create 131 in
         let state = ParserLog.mk_state
           ~offsets:options#offsets
           ~mode:options#mode
           ~buffer in
         begin
           ParserLog.print_tokens state ast;
           Buffer.output_buffer stdout buffer
         end
  with
    (* Lexing errors *)
    Lexer.Error err ->
      close_all ();
      let msg =
        Lexer.format_error ~offsets:options#offsets
                           options#mode err ~file
      in prerr_string msg

  (* Incremental API of Menhir *)
  | ParserFront.Point point ->
      let () = close_all () in
      let error =
        ParserFront.format_error ~offsets:options#offsets
                                 options#mode point
      in eprintf "\027[31m%s\027[0m%!" error

  (* Monolithic API of Menhir *)
  | Parser.Error ->
      let () = close_all () in
      let invalid, valid_opt =
        match get_win () with
          Lexer.Nil ->
            assert false (* Safe: There is always at least EOF. *)
        | Lexer.One invalid -> invalid, None
        | Lexer.Two (invalid, valid) -> invalid, Some valid in
      let point = "", valid_opt, invalid in
      let error =
        ParserFront.format_error ~offsets:options#offsets
                                 options#mode point
      in eprintf "\027[31m%s\027[0m%!" error

  (* I/O errors *)
  | Sys_error msg -> Utils.highlight msg
